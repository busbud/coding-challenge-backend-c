var url    = require('url');
var http   = require('http');
var path   = require('path');
var _      = require('lodash');
var async = require('async');
var Reader = require('./lib/reader');
var Search = require('./lib/search');

var port = process.env.PORT || 2345;
var cities_file_path = path.join(__dirname, 'data', 'cities.tsv');

// Send JSON response, takes object to serialize and http code
function sendResponse(res, data, http_code) {
  res.writeHead(http_code, {'Content-Type': 'application/json'});
  var json = JSON.stringify(data);
  res.end(json);
}

function startServer(callback) {
  async.waterfall([

      // Load cities
      function(done) {
        var citiesReader = new Reader();
        citiesReader.load(cities_file_path, function(cities) {
          done(null, cities);
        });
      },

      // Initialize search index with cities
      function(cities, done) {
        var search = new Search();
        search.add(cities);
        done(null, cities, search);
      },

      // Tell the search index to use memcache
      function(cities, search, done) {
        // TODO: Comming up!
        done(null, cities, search);
      },

      // Actually spin up the server handling the requests
      function(cities, search, done) {

        var server = http.createServer(function (req, res) {
          if (req.url.indexOf('/suggestions') === 0) {

            // Send suggestions
            var params = url.parse(req.url, true).query;
            var results = search.search(params);
            var status = results.length > 0 ? 200 : 404;

            sendResponse(res, {
              suggestions: results
            }, status);

          } else {

            // Send 404 Not Found
            sendResponse(res, {}, 404);

          }
        }).listen(port, '127.0.0.1');

        console.log('Server running at http://127.0.0.1:%d/suggestions', port);
        done(null, server);
      }
  ], function(err, server) {
    if (typeof callback != 'undefined') {
      callback(server);
    }
  });
}

if (!module.parent) {
  startServer();
}

// Like that we can use our server in tests
module.exports = startServer;


