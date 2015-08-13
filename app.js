var CitiesDb = require('./cities-db');
var http = require('http');
var path = require('path');
var url = require('url');

var host = process.env.HOST || '0.0.0.0';
var port = process.env.PORT || 2345;
var distanceScoreWeight = Number(process.env.DISTANCE_SCORE_WEIGHT) || 0.5;

// Create the cities DB
var citiesDb = new CitiesDb({
  distanceScoreWeight: distanceScoreWeight
});

// Create the HTTP server
var server = http.createServer(function (req, res) {

  // Endpoint: GET /suggestions
  if (req.url.indexOf('/suggestions') === 0) {

    // Validate method
    if (req.method !== 'GET') {
      sendJson(res, 405, {error: 'Method not allowed: ' + req.method});
      return;
    }

    // Validate parameters
    var params = url.parse(req.url, true).query;
    var q = '';
    var longitude = null;
    var latitude = null;
    if (params.q) {
      q = params.q;
      if (typeof q !== 'string') {
        sendJson(res, 400, {error: 'Malformed parameter: q'});
        return;
      }
    }
    if (params.latitude) {
      latitude = Number(params.latitude);
      if (isNaN(latitude)) {
        sendJson(res, 400, {error: 'Malformed parameter: latitude'});
        return;
      }
    }
    if (params.longitude) {
      longitude = Number(params.longitude);
      if (isNaN(longitude)) {
        sendJson(res, 400, {error: 'Malformed parameter: longitude'});
        return;
      }
    }

    // Generate response
    var suggestions = citiesDb.getSuggestions(params.q, latitude, longitude);
    var status = suggestions.length > 0 ? 200 : 404;
    sendJson(res, status, {suggestions: suggestions});
  }

  // Fallback: 404
  else {
    res.statusCode = 404;
    res.setHeader('Content-Type', 'text/plain');
    res.end('404 Not Found');
  }
});

module.exports = server;

// Populate the DB then start listening for requests
var here = path.dirname(module.filename);
citiesDb.readFile(
  path.join(here, 'data/cities_canada-usa.tsv'),
  function (err) {
    if (err) {
      console.log(err);
    }
    else {
      server.listen(port, host);
      console.log('Server running at http://%s:%d/suggestions', host, port);
    }
  }
);


/*
 * Build a JSON response
 */
function sendJson(res, status, obj) {
  var json = JSON.stringify(obj);
  res.setHeader('Content-Type', 'application/json; charset=utf-8');
  res.setHeader('Content-Length', json.length);
  res.statusCode = status;
  res.end(json);
}
