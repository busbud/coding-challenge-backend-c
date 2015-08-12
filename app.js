var http = require('http');
var host = process.env.HOST || '127.0.0.1';
var port = process.env.PORT || 2345;

module.exports = http.createServer(function (req, res) {
  res.writeHead(404, {'Content-Type': 'text/plain'});

  if (req.url.indexOf('/suggestions') === 0) {
    res.end(JSON.stringify({
      suggestions: []
    }));
  } else {
    res.end();
  }
}).listen(port, host);

console.log('Server running at http://%s:%d/suggestions', host, port);
