var citySearchEngine = require("./business/citySearchEngine");
var es = require("event-stream");
var events = require('events');
var fs = require("fs");
var http = require('http');
var url = require('url');

var searchEngine = null;
var port = process.env.PORT || 2345;
var server = http.createServer();

// add a custom event emitter on the server that will be used in the tests to know when server is ready
server.customEvents = new events.EventEmitter();

// handle incoming requests
server.on("request", function (req, res) {
    var urlInfo = url.parse(req.url, true);

    if (urlInfo.pathname === '/suggestions') {
        if (urlInfo.query.q) {
            searchEngine.searchStream(urlInfo.query)
                .pipe(es.stringify())
                .pipe(es.join(',\n'))
                .pipe(es.wait(function (err, data) {
                    res.writeHead(data ? 200 : 404, {'Content-Type': 'text/plain; charset=utf-8'});
                    res.end('{ "suggestions": [' + data + '] }');
                }));
        }
        else {
            res.writeHead(404, {'Content-Type': 'text/plain; charset=utf-8'});
            res.end('{ "suggestions": [] }');
        }
    }
    else if (urlInfo.pathname === '/help') {
        // provide a page that will help user
        fs.createReadStream("suggestions.html")
            .pipe(res)
            .pipe(es.wait(function(err,cb){
                res.writeHead(200, {'Content-Type': 'text/html; charset=utf-8'});
                res.end();
            }));
    }
    else {
        res.writeHead(404, {'Content-Type': 'text/plain'});
        res.end();
    }
});

// load and index cities in memory in order to speep up searches
citySearchEngine.init(/*"data/cities_canada-usa-lite.tsv",*/ function(engine) {
    searchEngine = engine;
    
    // start server
    server.listen(port, function() {
        console.log('Server running at http://127.0.0.1:%d/suggestions', port);
        server.isReady = true;
        server.customEvents.emit("ready");
    });
});

module.exports = server;