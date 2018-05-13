const lib = require("./lib");
const url = require("url");

lib.updateDatabase();

var http = require('http');
var port = process.env.PORT || 2345;

http.createServer(function (req, res) {
  if (req.url.indexOf('/suggestions') === 0) {
    const params = url.parse(req.url, true).query;
    res.writeHead(200, {'Content-Type': 'application/json'});
    lib.search(params)
      .then(suggestions => {
        res.end(JSON.stringify({
          suggestions
        }));
      })
      .catch(err => {
        res.end(JSON.stringify({Error: "Error searching for suggestions"}));
        console.log("Error searching for suggestions:", err);
      });
  } else {
    res.writeHead(404, {'Content-Type': 'text/plain'});
    res.end();
  }
}).listen(port);

console.log('Server running at http://127.0.0.1:%d/suggestions', port);
