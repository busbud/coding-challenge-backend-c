var citySearchEngine = require("./business/citySearchEngine");
var es = require("event-stream");
var http = require('http');
var url = require('url');

var searchEngine = null;
var port = process.env.PORT || 2345;
var server = http.createServer();

// handle incoming requests
server.on("request", function (req, res) {
    var urlInfo = url.parse(req.url, true);

    if (urlInfo.pathname === '/suggestions') {
        res.writeHead(200, {'Content-Type': 'text/plain'});
        searchEngine.searchStream(urlInfo.query)
            .pipe(es.stringify())
            .pipe(es.join(','))
            .pipe(es.wait())
            .pipe(es.mapSync(function(data){
                return "{suggestions:[" + data + "]}";
            }))
            .pipe(res);
    }
    else {
        res.writeHead(404, {'Content-Type': 'text/plain'});
        res.end();
    }
});

// start server
server.listen(port, '127.0.0.1', function() {
    console.log("Initializing...");
    
    // load and index cities in memory in order to speep up searches
    citySearchEngine.init(/*"data/cities_canada-usa-lite.tsv",*/ function(engine) {
        searchEngine = engine;
        console.log('Server running at http://127.0.0.1:%d/suggestions', port);
    });
});

module.exports = server;