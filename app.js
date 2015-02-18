var http = require('http');
var port = process.env.PORT || 2345;
var geoapi = require('./geoapi');

module.exports = http.createServer( function (req, res) {


  if (req.url.indexOf('/suggestions') === 0) {
    var query = require('url').parse(req.url, true).query;

    var geo = new geoapi('lclemence');

    geo.search(query, function (err, results){
      /*Handle errors*/
      if (err || results.length === 0){
        res.statusCode = 404;
        res.writeHead(404, {'Content-Type': 'text/plain'});
        res.end(JSON.stringify({
          suggestions: []
        }));

      }else{
        /*Display Geonames response*/
        res.writeHead(200, {'Content-Type': 'text/plain'});

        res.end(JSON.stringify({
          suggestions: results
        }));
      }

    });
  } else {
    res.end();
  }
}).listen(port, '127.0.0.1');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);