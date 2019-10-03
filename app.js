'use strict';

var http = require('http');
var port = process.env.PORT || 2345;
const url = require('url');
const Cities = require('./cities.js').Cities;
const cities = new Cities();

module.exports = http.createServer(function (req, res) {
  res.writeHead(404, {'Content-Type': 'text/plain'});
  const queryData = url.parse(req.url, true).query;

  if (req.url.indexOf('/suggestions') === 0) {
    res.writeHead(404, {'Content-Type': 'application/json'});
    var suggestions = [];
    if(queryData.q && queryData.longitude && queryData.latitude){
      suggestions = cities.queryWithLocation(queryData.q, parseFloat(queryData.longitude), parseFloat(queryData.latitude));
    }else if(queryData.q){
      suggestions = cities.queryWithoutLocation(queryData.q);
    }
    if(suggestions.length > 0){
      res.writeHead(200, {'Content-Type': 'application/json'});
    }
    res.end(JSON.stringify({
      suggestions: suggestions
    }));
  } else {
    res.end();
  }
}).listen(port, '0.0.0.0');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);