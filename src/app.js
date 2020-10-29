/**
 * Required Modules.
 */
const http = require('http');
const url = require('url');

const { readAndParseCSVFile } = require('./libs');
const { resourceNotFound } = require('./controllers');
const { routes } = require('./routes');

const port = process.env.PORT || 2345;
const listener = process.env.REQUEST_LISTENER || '0.0.0.0';

module.exports = http
  .createServer(async (req, res) => {
    const { pathname } = url.parse(req.url);
    const route = routes[pathname];

    // Persist the data in memory simulating a bootstrap process in order to make the process scalable.
    // A better option could be serializing the data and store it in a DB.
    const file = `${__dirname}/../data/cities_canada-usa.csv`; // Path to locate the .csv file.
    global.cities = await readAndParseCSVFile({ file }); // Read and parse the .csv file.

    // Set a default header.
    res.setHeader('Content-Type', 'application/json');

    // Validate all the routes within the GET verb.
    if (req.method === 'GET' && route) {
      route(req, res);
    } else {
      resourceNotFound(req, res);
    }
  })
  .listen(port, listener);

console.log('Server running at http://127.0.0.1:%d/suggestions', port);
