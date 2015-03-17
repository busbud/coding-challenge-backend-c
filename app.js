var http = require('http');
var url = require('url');

var store = require('./lib/store');
var Query = require('./lib/query');
var errors = require('./lib/errors');
var es = require('./lib/es');

var port = process.env.PORT || 2345;
var dataFile = './data/cities_canada-usa.tsv';
var apiHeaders = {
  'Content-Type': 'application/json; charset=utf-8',
  'Cache-Control': 'max-age=' +  3600 // 1 hour

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

  function parseQueryString(queryUrl) {
    return url.parse(queryUrl, true).query;
  }

  function sendSuggestions (err, suggestions) {
    if (err) {
      sendError(err);
    }
    res.writeHead(suggestions.length === 0 ? 404 : 200, apiHeaders);
    res.end(JSON.stringify({
      suggestions: suggestions
    }));
  }

  function getSuggestions () {
    var query = new Query(parseQueryString(req.url));
    store.connect(dataFile, function (err) {
      store.find(query, sendSuggestions);
    });
  }

  function getESSugguestions () {
    var query = new Query(parseQueryString(req.url));
    es.find(query, sendSuggestions);
  }

  // express... I miss you...
  res.writeHead(404, apiHeaders);
  if (req.url.indexOf('/suggestions') === 0 && req.method === 'GET') {
    try {
      getSuggestions(req, res);
    } catch (e) {
      sendError(e);
    }
  
  } else if (req.url.indexOf('/v2/suggestions') === 0 && req.method === 'GET') {
    getESSugguestions();
  } else {
    res.end();
  }
});
server.listen(port, function () {
  console.log('Server running on port', port);
});

module.exports = server;
