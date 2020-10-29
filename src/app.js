/**
 * Required Modules.
 */
const http = require('http');
const url = require('url');

const { resourceNotFound } = require('./controllers');
const { routes } = require('./routes');

const port = process.env.PORT || 2345;
const listener = process.env.REQUEST_LISTENER || '0.0.0.0';

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
      resourceNotFound(req, res);
    }
  })
  .listen(port, listener);

console.log('Server running at http://127.0.0.1:%d/suggestions', port);
