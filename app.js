var http = require('http');
var url = require('url');
var port = process.env.PORT || 2345;

// Modules init
var suggestions = require('./suggestions.js');
suggestions.loadData();

module.exports = http.createServer(function (req, res) {
    res.setHeader("Content-Type", "application/json; charset=utf-8");

    if (req.url.indexOf('/suggestions') === 0) {
        var query = url.parse(req.url, true).query;

        var results = suggestions.find(query.q, query.latitude, query.longitude);

        if(!results.length) {
            res.statusCode = 404;
        }

        res.end(JSON.stringify({
            suggestions: results
        }));
    } else {
        res.statusCode = 404;
        res.end();
    }
}).listen(port, '0.0.0.0');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);