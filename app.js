/* App will create 2 routes:
 * suggestions for the API
 * apitest for real-world testing of the API with a form text field
*/
var express = require('express');
var util = require('util');
var app = express();
var api_mongo = require('./api-mongo');
var api_stream = require('./api-stream');
var url = require('url');

// setup express environment
app.set('port', process.env.PORT || 2345);
app.set('views', __dirname + '/views');
app.set('view engine', 'jade');
app.use('/public', express.static(__dirname + '/public'));
app.locals.pretty = true;

// load properties from .env file (required by Heroku)
var dotenv = require('dotenv');
dotenv.load();
// load the environment
var PORT = process.env.PORT || 2345;
var API_URL = process.env.API_URL || 'localhost';
var USE_MONGO = process.env.USE_MONGO || 'false';
var DEBUG_MODE = process.env.DEBUG_MODE || 'false';

app.get('/suggestions', function(req, res) {
	res.setHeader('Content-Type','application/json');
	res.setHeader('Access-Control-Allow-Origin','*');
	// simple error checking
	var parts = url.parse(req.url, true);
	// ensure we have search text in the q argument
	if (parts.query['q'] == undefined || parts.query['q'].trim() == '') {
		res.status(404).end(JSON.stringify({'suggestions':{}}) + '\n');
	} else {
		// which method
		if (USE_MONGO == 'true') {
			api_mongo.handleRequest(req, res, parts);
		} else {
			api_stream.handleRequest(req, res, parts);
		}
	}
});

app.get('/apitest', function(req, res) {
    res.render('index', { 
    	API_URL: 'http://' + API_URL, 
    	title: 'API Endpoint Test Page', 
    	message: 'API Endpoint Test Page'
    });
});

var server = app.listen(PORT, function() {
	if (DEBUG_MODE == 'true') util.log(util.format('Server running at http://%s', API_URL));
	if (USE_MONGO == 'false') {
		if (DEBUG_MODE == 'true') util.log(util.format('Server configured to load data from streamed file.'));
		api_stream.loadData();
	} else {
		if (DEBUG_MODE == 'true') util.log(util.format('Server configured to use MongoDB.'));
	}
});

module.exports = server; // needed to run mocha tests