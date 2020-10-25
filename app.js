const http = require('http');
const url = require('url');

const port = process.env.PORT || 2345;

module.exports = http
  .createServer((req, res) => {
    const { pathname } = url.parse(req.url);

    res.writeHead(404, { 'Content-Type': 'text/plain' });

    if (pathname === '/suggestions') {
      res.end(
        JSON.stringify({
          suggestions: [],
        }),
      );
    } else {
      res.end();
    }
  })
  .listen(port, '127.0.0.1');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);
