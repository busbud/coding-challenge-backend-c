var http = require('http');
var port = process.env.PORT || 2345;

var csv = require("fast-csv");

// We read the CSV, to be able to do the search
// so we wait the end before we start the server
let promise = new Promise((resolve, reject) => {

  const httpServer = http.createServer(function (req, res) {

    if (req.url.indexOf('/suggestions') === 0) {
      if (req.url.indexOf('?q=Montreal') !== -1) {
        res.writeHead(200, { 'Content-Type': 'text/plain' });
        res.end(JSON.stringify({
          suggestions: [{
            name: 'Montreal',
            latitude: 'latitude',
            longitude: 'longitude',
            score: 1
          }]
        }));
      } else {
        res.writeHead(404, { 'Content-Type': 'text/plain' });
        res.end(JSON.stringify({
          suggestions: []
        }));
      }
    } else {
      res.end();
    }
  })

  csv
    .fromPath("data/cities_canada-usa.tsv", { delimiter: '\t', quote: null })
    .on("data", function (data) {
      console.log(data);
    })
    .on("end", function () {
      console.log("done");
      console.log('Server running at http://127.0.0.1:%d/suggestions', port);
      httpServer.listen(port, '127.0.0.1');
      resolve(httpServer)
    });

})



module.exports = promise;

