var fs = require('fs');
var http = require('http');
var port = process.env.PORT || 2345;
var parse = require('csv-parse');
var url = require('url');
var geolib = require('geolib');

const ZERO_DISTANCE = 400; //distance where score is only based on name

var output = [];
var suggestionMap = {}; //maps query term to list of ids
var cityMap = {}; //maps id to city entity
var canadianProvinceNameMap = {
  '01': 'AB',
  '02': 'BC',
  '03': 'MB',
  '04': 'NB',
  '05': 'NL',
  '07': 'NS',
  '08': 'ON',
  '09': 'PE',
  '10': 'QC',
  '11': 'SK',
  '12': 'YK',
  '13': 'NT',
  '14': 'NU'
};


var getScore = function(city, queryName, currLat, currLon){
  var nameScore = queryName.length/city.ascii.length;
  if(typeof(currLat) === 'undefined' || typeof(currLon) === 'undefined'){
    return Math.round(nameScore * 10) / 10; //round one decimal place
  }
  var distance = geolib.getDistance({
    latitude: currLat, 
    longitude: currLon
  }, {
    latitude: city.lat, 
    longitude: city.long
  })/1000; //distance in Km

  if(distance > ZERO_DISTANCE){ 
    var result = nameScore * (ZERO_DISTANCE / distance);
      return Math.round(result * 10) / 10;
  }

  var result = nameScore + ((1-nameScore) * (distance/ZERO_DISTANCE));
  return Math.round(result * 10) / 10; 
}

var input = fs.createReadStream('./data/cities_canada-usa.tsv');

var parser = parse({
  delimiter: '\t',
  columns: true
});

parser.on('readable', function () {
  while (record = parser.read()) {
    output.push(record);
  }
});

input.pipe(parser)
  .on('data', function (row) {
    if (parseInt(row.population) >= 5000 && (row.country === 'US' || row.country === 'CA')) {
      var city = {
        id: row.id,
        name: row.name,
        ascii: row.ascii.toLowerCase(),
        lat: row.lat,
        long: row.long,
        country: row.country === 'US' ? 'USA' : 'Canada',
        state: row.country === 'US' ? row.admin1 : canadianProvinceNameMap[row.admin1]
      };
      cityMap[city.id] = city;
      for (var i = city.name.length; i > 0; i--) {
        var searchKey = city.ascii.slice(0, i).toLowerCase();
        if (typeof suggestionMap[searchKey] === "undefined") {
          suggestionMap[searchKey] = [city.id];
        }
        else {
          suggestionMap[searchKey].push(city.id);
        }
      }
    }
  }).on('error', error => console.log(error));

  module.exports =
    parser.on('finish', function(){
      http.createServer(function (req, res) {
        var url_parts = url.parse(req.url, true);
        var query = url_parts.query;
        if (req.url.indexOf('/suggestions') === 0 && typeof(query.q) !== 'undefined') {
          var url_parts = url.parse(req.url, true);
          var query = url_parts.query;
          res.end(JSON.stringify({
            suggestions: suggestionMap[query.q.toLowerCase()].map(id => {
              return {
                name: `${cityMap[id].name}, ${cityMap[id].state}, ${cityMap[id].country}`,
                latitude: cityMap[id].lat,
                longitude: cityMap[id].long,
                score: getScore(cityMap[id], query.q, query.latitude, query.longitude)
              };
            })
          }));
        } else {
          res.end();
        }
      }).listen(port, '127.0.0.1');

      console.log('Server running at http://127.0.0.1:%d/suggestions', port);
    });
  
  


