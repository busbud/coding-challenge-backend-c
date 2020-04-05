const autocompleteSuggestions = require('./suggestionsFunctionality.js');

var http = require('http');
var port = process.env.PORT || 2345;

module.exports = http.createServer(function (req, res) {
  if (req.url.indexOf('/suggestions') === 0) {
    var temp = autocompleteSuggestions(req.url);
    if(temp.length == 0) {
      res.writeHead(404, {'Content-Type': 'text/plain'});
    } 
    res.end(JSON.stringify({
      suggestions: temp
    }));      
  } else {
    res.end();
  }
}).listen(port);

console.log('Server running at http://127.0.0.1:%d/suggestions', port);