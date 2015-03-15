var http = require('http');
var url = require('url');

var store = require('./lib/store');
var Query = require('./lib/query');
var errors = require('./lib/errors');

var port = process.env.PORT || 2345;
var dataFile = './data/cities_canada-usa.tsv';
var apiHeaders = {
  'Content-Type': 'application/json; charset=utf-8'
};



var server = http.createServer(function (req, res) {
  function sendError (e) {
    if (e instanceof errors.InvalidParameterError) {
      e.status = 400;
    } else {
      e.status = 500;
      e.errorMessage = 'An error occured'
    }
    // @TODO: add proper logging (winston)
    console.log('ERROR', e.status, '"' + req.method + ' ' + req.url + '"', e.errorMessage || e.message, e.stack);
    res.writeHead(e.status, apiHeaders);

    res.end(JSON.stringify({
      error: e.errorMessage || e.message
    }));
  }

  function getSuggestions() {
    var reqQuery = url.parse(req.url, true).query;
    var query = new Query(reqQuery);
    store.connect(dataFile, function (err) {

      store.find(query, function(err, suggestions) {
        if (err) {
          sendError(err);
        }
        res.writeHead(suggestions.length === 0 ? 404 : 200, apiHeaders);
        res.end(JSON.stringify({
          suggestions: suggestions
        }));
      });
    });
  }

  // express... I miss you...
  res.writeHead(404, apiHeaders);
  if (req.url.indexOf('/suggestions') === 0 && req.method === 'GET') {
    try {
      getSuggestions(req, res);
    } catch (e) {
      sendError(e);
    }
  
  } else {
    res.end();
  }
});
server.listen(port, '127.0.0.1', function () {
  console.log('Server running at http://127.0.0.1:%d/suggestions', port);
});

module.exports = server;
