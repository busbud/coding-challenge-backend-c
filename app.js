var http = require('http');
var port = process.env.PORT || 2345;
const url = require('url');
var fs = require("fs");
var parse = require('csv-parse/lib/sync');
var sigmoid = require('sigmoid');


// TODO: Initialize stream of file and convert into data structure that is optimized for searching
// TODO: Import some sort of geo API or find 3rd party (eg google)
function getResults(request) {
  var params = url.parse(request.url, true).query;

  // read from the file
  var dataString = fs.readFileSync("data/cities_canada-usa.tsv")
  // parse results to readable
  var results = parse(dataString, { quote: '', delimiter: "\t"});
  var newresults = results.filter(function (city) {
    return city[2].includes(params.q)
  });

  var newresultsFormatted = newresults.map(function(cityArray) {
    return {
      name: cityArray[2],
      latitude: cityArray[4],
      longitude: cityArray[5],
      score: getDistance(cityArray[4], cityArray[5], params.latitude, params.longitude)
    };
  });

  const sortedResults = newresultsFormatted.sort(function (a, b) {
    return b.score - a.score;
  });

  return { suggestions: sortedResults }

}

function getDistance(lat1,lon1,lat2,lon2) {
  var R = 6371; // Radius of the earth in km
  var dLat = deg2rad(lat2-lat1);  // deg2rad below
  var dLon = deg2rad(lon2-lon1);
  var a =
    Math.sin(dLat/2) * Math.sin(dLat/2) +
    Math.cos(deg2rad(lat1)) * Math.cos(deg2rad(lat2)) *
    Math.sin(dLon/2) * Math.sin(dLon/2)
    ;
  var c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1-a));
  var d = R * c; // Distance in km
  var sig = sigmoid(d / 100);
  return Math.round(sig * 100) / 10
}

function deg2rad(deg) {
  return deg * (Math.PI/180)
}

const server = http.createServer(function (req, res) {

  if (req.url.indexOf('/suggestions') === 0) {
    const results = getResults(req);

    console.log(results.suggestions.length)
    if (results.suggestions.length > 0) {
      res.writeHead(200, {'Content-Type': 'text/plain'});
    } else {
      res.writeHead(404, {'Content-Type': 'text/plain'});
    }

    const resultsAsString = JSON.stringify(results)

    res.end(resultsAsString);
  } else {
    res.end();
  }
});

server.listen(port, '127.0.0.1');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);

module.exports = server;
