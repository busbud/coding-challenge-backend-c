var http = require('http');
var port = process.env.PORT || 2345;

module.exports = http.createServer(function (req, res) {
  

  if (req.url.indexOf('/suggestions') === 0) {

    const suggestions = [];

    const returnCode = suggestions.length > 0 ? 200 : 404;

    res.writeHead(returnCode, {'Content-Type': 'application/json'});
    res.end(JSON.stringify({ suggestions: suggestions }));
  } else {
    res.writeHead(404, {'Content-Type': 'text/plain'});
    res.end();
  }
}).listen(port, '127.0.0.1');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);