/**
 * Required Modules.
 */
const http = require('http');
const url = require('url');

const { resourceNotFound } = require('./controllers');
const { routes } = require('./routes');

const port = process.env.PORT || 2345;

module.exports = http
  .createServer(async (req, res) => {
    const { pathname } = url.parse(req.url);
    const route = routes[pathname];

    // Set a default header.
    res.setHeader('Content-Type', 'application/json');

    // Validate all the routes within the GET verb.
    if (req.method === 'GET' && route) {
      await route(req, res);
    } else {
      await resourceNotFound(req, res);
    }
  })
  .listen(port, '127.0.0.1');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);
