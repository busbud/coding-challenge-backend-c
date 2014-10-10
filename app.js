var http = require('http');
var util = require('util');
var api = require('./api-logic');
// load properties from .env file since is used by Heroku
var dotenv = require('dotenv');
dotenv.load();
var port = process.env.PORT || 2345;
var host = process.env.HOST || '0.0.0.0'; // 0.0.0.0 allows access for locahost and local IP addresses

module.exports = http.createServer(function (req, res) {
	if (req.url.indexOf('/suggestions') === 0) {
		api.handleRequest(req,res);
	} else {
		res.writeHead(404, {'Content-Type': 'text/plain'});
		res.end();
	} 
}).listen(port, host);

util.log(util.format('Server running at http://%s:%d', host, port));
