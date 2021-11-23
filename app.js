const url = require('url');
var http = require('http');

const data = require("./data_searcher.js");

var port = process.env.PORT || 2345;

module.exports = http.createServer(function (req, res) {
  
  if (req.url.indexOf('/suggestions') === 0) {
    const query = url.parse(req.url,true).query;
    
    if(query.q) {
      data.searchDataSet(query.q, query.latitude, query.longitude, query.max_amount, query.loose_match).then((results) => {

        if(results.length > 0) {
          res.writeHead(200, {'Content-Type': 'application/json'});
        } else {
          res.writeHead(404, {'Content-Type': 'application/json'});
        }
  
        res.end(JSON.stringify({
          suggestions: results
        }));

      });

    } else {
      res.writeHead(404, {'Content-Type': 'application/json'});

      res.end(JSON.stringify({
        suggestions: []
      }));
    }
    
  } else {
    res.writeHead(404, {'Content-Type': 'text/plain'});
    res.end();
  }
}).listen(port);

console.log('Server running at http://127.0.0.1:%d/suggestions', port);