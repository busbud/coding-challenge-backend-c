"use strict";
var http = require('http'),
    querystring = require('querystring'),
    port = process.env.PORT || 2345,
    cities = require('./cities');

module.exports = http.createServer(function (req, res) {
    var query = querystring.parse(req.url.split('?')[1]),
        results;
    if (req.url.indexOf('/suggestions') === 0) {
        results = cities.suggestions(query.q, query.longitude, query.latitude);
        res.writeHead(results.length > 0 ? 200 : 404, {'Content-Type': 'application/json; charset=utf-8'});
        res.end(JSON.stringify({
            suggestions: results
        }));
    } else {
        res.writeHead(404, {'Content-Type': 'text/plain'});
        res.end();
    }
}).listen(port, '127.0.0.1');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);
