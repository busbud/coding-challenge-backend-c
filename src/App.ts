import http, { IncomingMessage, ServerResponse } from 'http';
var port = process.env.PORT || 2345;

export default http.createServer(function (req: IncomingMessage, res: ServerResponse) {
  res.writeHead(404, { 'Content-Type': 'text/plain' });

  if (req.url && req.url.indexOf('/suggestions') === 0) {
    res.end(JSON.stringify({
      suggestions: []
    }));
  } else {
    res.end();
  }
}).listen(port);
console.log('Server running at http://127.0.0.1:%d/suggestions', port);
