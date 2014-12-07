//Starts the autocomplete API server
var http = require('http');
var url=require('url');

exports.go = function (port,responder,search_structure) {
    http.createServer(function (req, res) {
        res.writeHead(200, { 'Content-Type': 'application/json' });
        
        if (req.url.indexOf('/suggestions') === 0) {
            res.end(responder.getResponseString(url.parse(req.url,true),search_structure));
        } else {
            res.end();
        }
    }).listen(port, '127.0.0.1');
    
    console.log('Server running at http://127.0.0.1:%d/suggestions', port);
};