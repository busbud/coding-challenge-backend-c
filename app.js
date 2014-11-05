var port   = process.env.PORT || 2345;
var http   = require('http');
var fs     = require('fs');
var es     = require('event-stream');
var url    = require('url');
var geolib = require('geolib');

var CA_STATES = {
  '01': 'AB',
  '02': 'BC',
  '03': 'MB',
  '04': 'NB',
  '05': 'NL',
  '07': 'NS',
  '08': 'ON',
  '09': 'NS',
  '10': 'QB',
  '11': 'SK',
  '12': 'YK',
  '13': 'NT',
  '14': 'NU'
}

module.exports = http.createServer(function (req, res) {
  
  if (req.url.indexOf('/suggestions') === 0) {

    var suggestions     = [];
    var url_parts       = url.parse(req.url, true);
    var search          = escapeRegExp(url_parts.query.q.trim());
    var clientLatitude  = url_parts.query.latitude;
    var clientLongitude = url_parts.query.longitude;
    var regex           = new RegExp('^' + search, 'i');

    var input = fs.createReadStream('data/cities_canada-usa.tsv');

    input
    .on('end', function() {
      if (suggestions.length > 0) {
        res.writeHead(200, {'Content-Type': 'application/json'});    
        suggestions.sort(compare);
        res.end(JSON.stringify({ suggestions: suggestions}));
      } else {
        res.writeHead(404, {'Content-Type': 'application/json'});
        res.end(JSON.stringify({ suggestions: suggestions}));
      }
    });

    input
    .pipe(es.split('\n'))
    .pipe(es.mapSync(function (data) {
      return data.split('\t');
    }))
    .pipe(es.mapSync(function (data) {
      if (data.length >= 11){
        var name = data[2].trim();
        if (name.match(regex) && name.length > 0) {
          var cityLatitude  = data[4];
          var cityLongitude = data[5];
          if (typeof clientLatitude !== 'undefined' && typeof clientLongitude !== 'undefined') {
            var distance = geolib.getDistance(
              {latitude: clientLatitude, longitude: clientLongitude}, 
              {latitude: cityLatitude,   longitude: cityLongitude}
            );
            var regexMatch = search.length / name.length;
            var score = finalScore(distanceScore(distance), regexMatch);
          } else {
            var score = search.length / name.length;
          }
          name = formatName(name, data);
          var suggestion = {
            name: name,
            latitude: cityLatitude,
            longitude: cityLongitude,
            score: score
          }
          suggestions.push(suggestion);
        }
      }
    }))
  } else {
    res.end();
  }
}).listen(port, '127.0.0.1');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);

function escapeRegExp(string) {
  return string.replace(/([.*+?^${}()|\[\]\/\\])/g, "\\$1");
}

function distanceScore(distance) {
  return 1 - (distance / 4800000);
}

function finalScore(distanceScore, regexScore) {
  return distanceScore / 2 + regexScore / 2;
}

function compare(a,b) {
  if (a.score > b.score) {
    return -1;
  }
  if (a.score < b.score) {
    return 1;
  }
  return 0;
}

function formatName(name, data) {
  var state = '';
  var country = '';
  if (data[10] in CA_STATES) {
    state = CA_STATES[data[10]];
  } else {
    state = data[10];
  }
  if (data[8] === 'US') {
    country = 'USA'
  }
  else if (data[8] === 'CA'){
    country = 'Canada'
  } 
  return name + ', ' + state + ', ' + country  
}

