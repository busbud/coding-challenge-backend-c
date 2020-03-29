var http = require('http');
var port = process.env.PORT || 2345;

module.exports = http.createServer(function (req, res) {
  if (req.url.indexOf('/suggestions') === 0) {
    res.writeHead(404, { 'Content-Type': 'json/plain' });
    res.end(JSON.stringify({
      suggestions: []
    }));
  } else {
    res.end();
  }
}).listen(port, '127.0.0.1');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);
