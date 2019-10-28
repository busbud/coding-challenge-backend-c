var http = require('http');
var port = process.env.PORT || 2345;

const routes = require('./routes');

module.exports = http.createServer(function (req, res) {
  routes(req, res);
}).listen(port, '127.0.0.1');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);
