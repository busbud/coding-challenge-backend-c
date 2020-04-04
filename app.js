const autocompleteSuggestions = require('./suggestionsFunctionality.js');

var http = require('http');
var port = process.env.PORT || 2345;

module.exports = http.createServer(function (req, res) {
  //res.writeHead(404, {'Content-Type': 'text/plain'});
  if (req.url.indexOf('/suggestions') === 0) {
    res.end(JSON.stringify({
      suggestions: autocompleteSuggestions(req.url)
    }));
  } else {
    res.end();
  }
}).listen(port);

console.log('Server running at http://127.0.0.1:%d/suggestions', port);