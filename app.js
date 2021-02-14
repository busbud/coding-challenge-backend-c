const http = require('http');
const port = process.env.PORT || 3101;
const url = require('url');

const { parseDataSource, findObjects, sortResults } = require('./helpers');
module.exports = http
  .createServer(async function (req, res) {
    res.writeHead(404, { 'Content-Type': 'text/plain' });

    if (req.url.indexOf('/suggestions') === 0) {
      const params = url.parse(req.url, true).query;

      try {
        const result = await parseDataSource();
        const dataTo = findObjects(params.q, result);
        let coordinateQuery = {};
        if (params.latitude && params.longitude) {
          coordinateQuery = {
            coordinates: { lat: params.latitude, lon: params.longitude },
          };
        } else {
          coordinateQuery = params.q;
        }
        const sortedResults = await sortResults(coordinateQuery, dataTo);

        res.end(
          JSON.stringify({
            suggestions: sortedResults,
          })
        );
      } catch (e) {
        console.log(e);
      }
    } else {
      res.end();
    }
  })
  .listen(port, '127.0.0.1');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);
