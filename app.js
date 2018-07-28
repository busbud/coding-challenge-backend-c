const http = require('http');
const url = require('url');
const port = process.env.PORT || 2345;
const mongoURI = process.env.MONGO_URI || 'mongodb://localhost/busbud';
const MongoClient = require('mongodb').MongoClient;
const CityService = require('./lib/city.service');
const ProvinceService = require('./lib/province.service');
const ScoreService = require('./lib/score.service');

// connect to mongo and start the http server
const start = async () => {
  const client = await MongoClient.connect(mongoURI);

  const scoreService = new ScoreService();
  const provinceService = new ProvinceService(client);
  const cityService = new CityService(client, scoreService, provinceService);

  return http
    .createServer(async (req, res) => {
      try {
        if (req.url.indexOf('/suggestions') === 0) {
          const query = url.parse(req.url, true).query;
          const cities = await cityService.findCities(query);

          // if no matching city is found, status code is 404
          const statusCode = cities.length > 0 ? 200 : 404;

          res.writeHead(statusCode, { 'Content-Type': 'application/json' });
          res.end(
            JSON.stringify({
              suggestions: cities
            })
          );
        } else {
          res.writeHead(404, { 'Content-Type': 'text/html' });
          res.end();
        }
      } catch (e) {
        console.error(e);
        res.writeHead(400, { 'Content-Type': 'application/json' });
        res.end(
          JSON.stringify({
            error: e.message
          })
        );
      }
    })
    .listen(port);
};

module.exports = start().then(s => {
  console.log('Server running at http://127.0.0.1:%d/suggestions', port);
  return s;
});
