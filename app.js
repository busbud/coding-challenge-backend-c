const http = require('http');
const port = process.env.PORT || 2345;
const url = require('url');
const querystring = require('querystring');
const cityRequester = require("./modules/city-requester");

module.exports = http.createServer(function(req, res) {
    res.setHeader('Content-Type', 'application/json; charset=utf-8');
    if (req.url.indexOf('/suggestions') !== 0) {
        res.statusCode = 404;
        res.end(JSON.stringify({'error': 'route not implemented'}));
        return;
    }
    const requestQuery = querystring.parse(url.parse(req.url).query);
    const suggestionsQuery = {
        destination: requestQuery.q,
        latitude: requestQuery.latitude,
        longitude: requestQuery.longitude
    };
    cityRequester.getSuggestions(suggestionsQuery)
        .then(suggestions => {
            res.statusCode = 200;
            res.end(JSON.stringify({suggestions: suggestions}));
        })
        .catch(err => {
            res.statusCode = err.status || 500;
            res.end(JSON.stringify({'error': err.error || err.message}));
        });

}).listen(port, process.env.PROD ? null : '127.0.0.1');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);
