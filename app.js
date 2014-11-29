var http = require('http');
var port = process.env.PORT || 2345;

var routesSuggestions = require('./routes/suggestions')
var routesWildcard = require('./routes/wildcard')

module.exports = http.createServer(function (req, res) {
  if (req.url.indexOf('/suggestions') === 0) {
    routesSuggestions(req, res);
  } else {
    routesWildcard(req, res);
  }
}).listen(port, '127.0.0.1');
console.log('Server running at http://127.0.0.1:%d/suggestions', port);