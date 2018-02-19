const http = require('http');
const port = process.env.PORT || 2345;
const host = process.env.HOST || '127.0.0.1';

const { connectToES } = require('./utility/elasticSearch');
const SuggestionsController = require('./controllers/SuggestionsController');

const esOptions = {
    apiVersion: process.env.ES_VERSION || "5.3",
    host: process.env.BONSAI_URL || "http://localhost:9200"
};

connectToES(esOptions).then(esClient => {
// server definition
  const suggestionsController = new SuggestionsController(esClient);

  const server = http.createServer((req, res) => {
    if (req.url.indexOf('/suggestions') === 0) {
      if (req.method === 'GET') suggestionsController.get(req, res);
    } else {
      res.writeHead(404, {'Content-Type': 'text/plain'});
      res.end();
    }
  });

// start the server
  server.listen(port, host, () => {
    console.log('Server running at http://%s:%d/suggestions', host, port);
  });
});