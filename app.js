var http = require('http');
var url = require('url') ;
var port = process.env.PORT || 2345;

var suggestionsEngine = require(__dirname+'/suggestions.js');

module.exports = http.createServer(function (req, res) {
  if (req.url.indexOf('/suggestions') === 0) {
    try {
      // Quick and easy profiling for the runtime
      // console.time("SuggestionEngine Runtime");
      var query = url.parse(req.url, true).query;
      var suggestions = suggestionsEngine.get(query.q, query.latitude, query.longitude);
      if (suggestions.length <= 0) {
        res.writeHead(404, {'Content-Type': 'application/json'});
      } else {
        res.writeHead(200, {'Content-Type': 'application/json'});
      }
      res.end(JSON.stringify({ suggestions: suggestions }));
      // console.timeEnd("SuggestionEngine Runtime");
    } catch(err) {
      // Do not crash the server, instead just log and return empty suggestions
      console.error(err);
      res.writeHead(400, {'Content-Type': 'application/json'});
      res.end(JSON.stringify({ suggestions: [] }));
    }
  } else {
    res.writeHead(404, {'Content-Type': 'text/plain'});
    res.end();
  }
}).listen(port);

console.log('Server running on port %d', port);
