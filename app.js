import citiesController from './src/controller/cities.controller';

var http = require('http');
var port = process.env.PORT || 2345;

module.exports = http.createServer(function (req, res) {
  res.writeHead(404, { 'Content-Type': 'text/plain' });

  if (req.url.indexOf('/suggestions') === 0) {
      return citiesController.suggestions(req.url, res)
  } else {
    res.end();
  }
}).listen(port, '0.0.0.0');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);