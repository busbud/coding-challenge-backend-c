const http = require('http');
const port = process.env.PORT || 2345;
const suggestionsController = require('./controllers/suggestions');
mongo = require('./mongo');
mongo.connect();

module.exports = http.createServer(function (req, res) {

    if (req.url.indexOf('/suggestions') === 0) {
        return suggestionsController.handleRequest(req, res);
    } else {
        res.end();
    }
}).listen(port, '127.0.0.1');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);