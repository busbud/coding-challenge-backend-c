var http   = require('http');
var path   = require('path');
var _      = require('lodash');
var Reader = require('./lib/reader');
var async = require('async');

var port = process.env.PORT || 2345;
var cities_file_path = path.join(__dirname, 'data', 'cities.tsv');

async.waterfall([
    function(done) {
      var citiesReader = new Reader();
      citiesReader.load(cities_file_path, function(cities) {
        done(null, cities);
      });
    },
    function(cities, done) {

      module.exports = http.createServer(function (req, res) {
        if (req.url.indexOf('/suggestions') === 0) {

          // Send suggestions
          res.writeHead(200, {'Content-Type': 'application/json'});
          res.end(JSON.stringify({
            suggestions: _.map(cities.slice(0,5), function(i) { return i.toObject(); })
          }));

        } else {

          // Send 404 Not Found
          res.writeHead(404, {'Content-Type': 'text/plain'});
          res.end();

        }
      }).listen(port, '127.0.0.1');

      console.log('Server running at http://127.0.0.1:%d/suggestions', port);
      done(cities);
    }
]);


