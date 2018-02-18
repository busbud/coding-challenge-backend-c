const http = require('http');
const port = process.env.PORT || 2345;
const host = process.env.HOST || '127.0.0.1';

const SuggestionsController = require('./controllers/SuggestionsController');

// import data
const db = {};
// instantiate the suggestions controller with the db
const suggestionsController = new SuggestionsController(db);

// server definition
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