var cityIndex = require('./business/cityIndex');
var cityLoader = require("./business/cityLoader");
var http = require('http');
var url = require('url');

var cityStore = cityIndex.createIndex();
var port = process.env.PORT || 2345;
var server = http.createServer();

server.on("request", function (req, res) {
    if (req.url.indexOf('/suggestions') === 0) {
        var params = url.parse(req.url, true);
        console.log("requestion with ", params.q);
        var cities = cityStore.findCities(params.query.q);
        res.end(JSON.stringify({ suggestions: cities }));
        res.writeHead(200, {'Content-Type': 'text/plain'});
    }
    else {
        res.writeHead(404, {'Content-Type': 'text/plain'});
        res.end();
    }
});

server.listen(port, '127.0.0.1', function() {
    console.log("Initializing...");
    
    var readStream = cityLoader.loadAndStoreTo(cityStore);

    readStream.on("end", function() {
      console.log('Server running at http://127.0.0.1:%d/suggestions', port);
    });
});

module.exports = server;