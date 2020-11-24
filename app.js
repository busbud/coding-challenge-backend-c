var http = require('http');
var port = process.env.PORT || 2345;

module.exports = http.createServer(function (req, res) {
  res.writeHead(404, { 'Content-Type': 'text/plain' });

  if (req.url.indexOf('/suggestions') === 0) {
    res.end(JSON.stringify({
      suggestions: []
    }));
  } else {
    res.end();
  }
}).listen(port, '0.0.0.0');

console.log('Server running at http://0.0.0.0:%d/suggestions', port);