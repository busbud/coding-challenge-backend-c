/* App will create 2 routes:
 * suggestions for the API
 * apitest for real-world testing of the API with a form text field
*/

var express = require('express');
var util = require('util');
var app = express();
var api = require('./api-logic');

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

app.get('/suggestions', function(req, res) {
	res.setHeader('Content-Type','application/json');
	res.setHeader('Access-Control-Allow-Origin','*');
	api.handleRequest(req,res);
});

app.get('/apitest', function(req, res) {
    res.render('index', { 
    	API_URL: 'http://' + API_URL, 
    	title: 'API endpoint test page', 
    	message: 'API endpoint test page'
    });
});

var server = app.listen(PORT, function() {
	util.log(util.format('Server running at http://%s', API_URL));
});
