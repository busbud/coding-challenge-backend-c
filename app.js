var http = require('http');
var port = process.env.PORT || 2345;

// Thom
var suggestionsController = require('./controllers/suggestions');
const dataFileName        = process.env.CITIES_FILE_NAME || 'cities_canada-usa';
var parser                = require('./services/cities-parser');

// Generate data at server start
var data;
parser(dataFileName).then(fileData => {
  data = fileData;
});

module.exports = http.createServer(function (req, res) {
  res.writeHead(404, {'Content-Type': 'text/plain'});

  if (req.url.indexOf('/suggestions') === 0) {
    return suggestionsController(req, res, data);
  } else {
    res.end();
  }
}).listen(port, '127.0.0.1');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);