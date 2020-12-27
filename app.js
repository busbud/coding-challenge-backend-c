const http = require('http');
const port = process.env.PORT || 2345;

const csv     = require('csv-parser');
const fs      = require('fs');
const url     = require('url');
const geolib  = require('geolib');
const _       = require('lodash');

module.exports = http.createServer(function (req, res) {
  try {
    if (req.url.indexOf('/suggestions') === 0) {
      const params = url.parse(req.url,true).query;
      let suggestions = [];

      // no query param => nothing to search for
      if (!params.q) {
        throw new Error('No search query parameter provided.');
      }

      // query param to short
      if (params.q.length < 3) {
        throw new Error('Search query is too short.');
      }

      // check if data file is provided
      let filename = 'data/cities_canada-usa.tsv';
      if (!fs.existsSync(filename)) {
        throw new Error('Data source file not found');
      }

      // regular expression to check for a match
      let reg = new RegExp("([^,]*)" + params.q + "([^,]*)", 'gi');

      // reading a data file with a stream
      fs.createReadStream(filename)
      .pipe(csv({ separator: '\t' }))
      .on('data', (data) => {

        // check againts name and alternative names of the cities excluding small cities
        if ((data.name.match(reg) || data.alt_name.match(reg)) && parseInt(data.population) > 5000) {
          // get difference between length or search string and found match
          let matchedName = !data.name.match(reg) && data.alt_name.match(reg) ? 
              data.alt_name.match(reg)[0] : 
              data.name;
          let searchDiff = matchedName.length - params.q.length;

          // calculate distance if latitude and longitude parameters are present
          let distance  = params.latitude && params.longitude ? geolib.getDistance(
            {latitude: data.lat, longitude: data.long},
            {latitude: params.latitude, longitude: params.longitude},
          ) : 0;

          // check if searched word is the beginning of the matched result
          let searchPos  = matchedName.toLowerCase().indexOf(params.q.toLowerCase()) === 0 ? 1 : 0;
          console.log('matchedName: ', matchedName);
          console.log('matchedName.indexOf(params.q): ', matchedName.toLowerCase().indexOf(params.q.toLowerCase()));

          // form result with the all necessary info
          suggestions.push({
            name: data.name + ', ' + data.country,
            latitude: data.lat,
            longitude: data.long,
            distance: distance,
            searchDiff: searchDiff,
            searchPos: searchPos
          });
        }
      })
      .on('end', () => {
        if (suggestions.length > 0) {
          // sort by distance and search / result match difference
          suggestions = _.orderBy(suggestions, ['distance', 'searchPos', 'searchDiff'], ['asc', 'asc', 'asc']);

          let n = suggestions.length;
          let i = 0;
          let maxScore = 1;
          // basic score calculation
          suggestions.map(el => {
            el.score = parseFloat((maxScore * (1- i/n)).toFixed(2));
            delete el.distance;
            delete el.searchDiff;
            delete el.searchPos;
            i++;
            return el;
          });
          // all good => return result with 200 OK
          res.writeHead(200, {'Content-Type': 'text/html'});
          res.end(JSON.stringify({
            suggestions: suggestions
          }));
        } else {
          // nothing found => return empty array with 404 Not Found
          res.writeHead(404, {'Content-Type': 'text/plain'});
          res.end(JSON.stringify({
            suggestions: []
          }));
        }
      });
      
    } else {
      res.end();
    }

  } catch (e) {
    res.writeHead(500, {'Content-Type': 'text/plain'});
    res.end(JSON.stringify({
      error: e.message
    }));
  }
}).listen(port, '127.0.0.1');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);