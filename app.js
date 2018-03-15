var http = require('http');
var url = require('url');
var port = process.env.PORT || 2345;
var serverHelper = require('./lib/serverHelpers');


const util = new serverHelper.util();

module.exports = http.createServer(function (req, res) {
    res.writeHead(404, {'Content-Type': 'text/plain'});

    if (req.url.indexOf('/suggestions') === 0) {
        // Ugly Promise sytnax but didn't want to re-wrte app tests
        // Would be much nice to user async/await
        util.thottleConnection(req)
            .then(function (release) {
                return util.getSuggestionsFromRequest(url.parse(req.url, true).query);
            })
            .then(function (data) {
                if (data.length)
                    res.writeHead(200);
                else
                    res.writeHead(404);

                res.end(JSON.stringify({
                    suggestions: data
                }));
            })
            .catch(function (err) {
                res.writeHead(401)
                res.end(JSON.stringify(err));
            })

    } else {
        res.end();
    }
}).listen(port, '127.0.0.1');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);