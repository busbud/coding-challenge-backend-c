/**
 * Required Modules.
 */
const http = require('http');
const url = require('url');

const { filterCities, readAndParseCSVFile } = require('./helpers');

const port = process.env.PORT || 2345;

module.exports = http
  .createServer(async (req, res) => {
    const { pathname, query } = url.parse(req.url, true);

    res.setHeader('Content-Type', 'application/json');

    if (req.method === 'GET' && pathname === '/suggestions') {
      // Path to locate the .csv file.
      const file = './data/cities_canada-usa.csv';

      // Read and parse the .csv file.
      const cities = await readAndParseCSVFile({ file });

      // Create the suggestions based on the query filters.
      const suggestions = filterCities({
        cities,
        name: query.q,
        latitude: Number(query.latitude),
        longitude: Number(query.longitude),
      });

      // Default status code for an empty array.
      let statusCode = 404;
      if (suggestions.length > 0) {
        // Set the status for records found.
        statusCode = 200;

        // Sort suggestions by descending score.
        suggestions.sort((a, b) => b.score - a.score);
      }

      res.statusCode = statusCode;
      res.end(JSON.stringify({ suggestions }));
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
