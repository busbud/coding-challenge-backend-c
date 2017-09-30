const http = require('http');
const port = process.env.PORT || 2345;

// Thom
const suggestionsController = require('./controllers/suggestions');
require('./mongo-init');

module.exports = http.createServer(function (req, res) {
    res.writeHead(404, { 'Content-Type': 'text/plain' });

    if (req.url.indexOf('/suggestions') === 0) {
        return suggestionsController(req, res);
    } else {
        res.end();
    }
}).listen(port, '127.0.0.1');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);