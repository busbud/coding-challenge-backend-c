var http = require('http');

var search =  require('./search');

var port = process.env.PORT || 2345;

module.exports = http.createServer(function (req, res) {
  res.writeHead(404, {'Content-Type': 'text/plain'});

  if (req.url.indexOf('/suggestions') === 0) {
    res.writeHead(200, {'Content-Type': 'application/json; charset=utf-8'});
    var result = {};
    result.suggestions = search.search(req.url);
    if(result.suggestions.length == 0) {
        res.writeHead(404, {'Content-Type': 'application/json; charset=utf-8'});
    }
    res.end(JSON.stringify(result,null," "));
  } else {
    res.end();
  }
}).listen(port);

console.log('Server Started!');
