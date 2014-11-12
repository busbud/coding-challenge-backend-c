var http = require('http');
var port = process.env.PORT || 2345;
var url = require('url');

var suggestions = require('./src/suggestions.js');

module.exports = http.createServer(function (req, res) {
  if (req.url.indexOf('/suggestions') === 0) {
    var queryString = url.parse(req.url, true).query;
    suggestions.get(queryString, function (err, suggestions) {
        if (err) {
            res.writeHead(404, {'Content-Type': 'application/json'});
        } else {
            res.writeHead(200, {'Content-Type': 'application/json'});
        }
        
        res.end(JSON.stringify({
            suggestions: suggestions
        }));
    });
  } else {
    res.writeHead(404, {'Content-Type': 'text/plain'});
    res.end();
  }
}).listen(port);//, '127.0.0.1');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);
