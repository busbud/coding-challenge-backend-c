var http = require('http');
var port = process.env.PORT || 2345;
var getSuggestions = require(__dirname + '/suggestions/getSuggestions.js');

module.exports = http.createServer(function (req, res) {
  res.writeHead(404, {'Content-Type': 'text/plain'});

  if (req.url.indexOf('/suggestions') === 0) {
    var request = new URL(req.url, `http://${req.headers.host}`);
    if (request && request.searchParams && request.searchParams.get('q')) {

      /********** Dumb hack-y solution to keep the connection open longer than heroku's max 30s *******/ 
      res.write(' ')
      var keepOpen = setInterval(function () {
        res.write(' ')
      }, 30000)
      /********** End of dumb hack-y solution :(:(:( JSON parse will trim the whitespace ************/

      getSuggestions(request.searchParams)
      .then(function (result) {
        clearInterval(keepOpen);
        if (result.suggestions.length >= 0) {
          res.statusCode = 200
          res.end(JSON.stringify({suggestions: result.suggestions}));
        }
        else {
          res.statusCode = 404;
          res.end(JSON.stringify({suggestions: []}));
        }
      })
      .catch(function (err) {
        clearInterval(keepOpen);
        res.end();
      })
    }
    else {
      res.end();
    }
  } else {
    res.end();
  }
}).listen(port);

console.log('Server running at http://127.0.0.1:%d/suggestions', port);