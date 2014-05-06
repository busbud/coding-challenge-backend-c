var http = require('http');
var RateLimiter = require('limiter').RateLimiter;

var search =  require('./search');

var limiter = new RateLimiter(1, 250);

var port = process.env.PORT || 2345;

module.exports = http.createServer(function (req, res) {
  limiter.removeTokens(1, function(err, remainingRequests) {
  if (remainingRequests < 0) {
    response.writeHead(429, {'Content-Type': 'text/plain;charset=UTF-8'});
    response.end('429 Too Many Requests - your IP is being rate limited');
  } else {
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
    }
  });
}).listen(port);

console.log('Server Started!');
