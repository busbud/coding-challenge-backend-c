const http = require('http');
    port = process.env.PORT || 2345,
    suggestionsController = require('./controllers/suggestions'),
    toobusy = require('toobusy-js');
mongo = require('./mongo');
mongo.connect();

module.exports = http.createServer(function (req, res) {

    // Settings with a small library for high traffic volume
    toobusy.maxLag(10);
    toobusy.interval(250);
    toobusy.onLag((currentLag) => {
        res.statusCode = 503;
        return res.end(JSON.stringify({
            suggestions: []
        }));
      });
    

    if (req.url.indexOf('/suggestions') === 0) {
        return suggestionsController.handleRequest(req, res);
    } else {
        res.end();
    }
}).listen(port, '127.0.0.1');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);