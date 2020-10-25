const http = require('http');
const url = require('url');

const port = process.env.PORT || 2345;

module.exports = http
  .createServer((req, res) => {
    const { pathname } = url.parse(req.url);

    res.setHeader('Content-Type', 'application/json');

    if (req.method === 'GET' && pathname === '/suggestions') {
      res.statusCode = 200;
      res.end(
        JSON.stringify({
          suggestions: [],
        }),
      );
    } else {
      res.statusCode = 404;
      res.end(
        JSON.stringify({
          message: 'Oops! Something went wrong! The requested resource was not found.',
        }),
      );
    }
  })
  .listen(port, '127.0.0.1');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);
