const _ = require('lodash');
const http = require('http');
const port = process.env.PORT || 2345;
const host = process.env.HOST || '127.0.0.1';

const { connectToES } = require('./utility/elasticSearch');
const SuggestionsController = require('./controllers/SuggestionsController');

const esConfig = require('./esConfig.json');
const _esConfig = _.merge({}, esConfig, {
  apiVersion: process.env.ES_VERSION || esConfig.apiVersion,
  host: process.env.BONSAI_URL || esConfig.host
});

module.exports = connectToES(_esConfig).then(esClient => {
// server definition
  const suggestionsController = new SuggestionsController(esClient);

  const httpServer = http.createServer((req, res) => {
    if (req.url.indexOf('/suggestions') === 0) {
      if (req.method === 'GET') suggestionsController.getCities(req, res);
    } else {
      res.writeHead(404, {'Content-Type': 'text/plain'});
      res.end();
    }
  });

// start the server
  return Promise.resolve(httpServer.listen(port, host, () => {
    console.log('Server running at http://%s:%d/suggestions', host, port);
  }));
});