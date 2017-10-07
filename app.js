const http = require('http');
const suggestionsHanlder = require('./handlers/suggestions');

const port = process.env.PORT || 2345;

module.exports = http
  .createServer((req, res) => {
    if (req.url.indexOf('/suggestions') === 0) {
      const { status = 404, headers = {}, body = {} } = suggestionsHanlder(req);

      res.writeHead(status, headers);
      res.end(JSON.stringify(body));
    } else {
      res.writeHead(404, { 'Content-Type': 'text/plain' });
      res.end();
    }
  })
  .listen(port, '0.0.0.0');

console.log('Server running at http://0.0.0.0:%d/suggestions', port);
