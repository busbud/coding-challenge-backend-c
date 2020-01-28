var http = require('http');
var url = require('url');
var suggestions = require('./suggestions.js');

var port = process.env.PORT || 2345;


function serveSuggestions(req, res) {
  var reqUrl = url.parse(req.url, true);
  var body = {
    suggestions: suggestions.getSuggestions(reqUrl.query)
  };
  // Set headers, send response
  res.statusCode = body.suggestions.length > 0 ? 200 : 404;
  res.statusMessage = body.suggestions.length > 0 ? 'OK' : 'No results found';
  res.setHeader('Content-Type', 'application/json');
  res.end(JSON.stringify(body));
}

module.exports = http.createServer(function (req, res) {

  if (req.url.indexOf('/suggestions') === 0) {
    serveSuggestions(req, res);
  } else {
    res.end();
  }
}).listen(port, '127.0.0.1');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);