// App will create 2 routes:
// suggestions for the API
// apitest for testing the API

var express = require('express');
var util = require('util');
var app = express();
var api = require('./api-logic');

// setup express environment
app.set('port', process.env.PORT || 3000);
app.set('views', __dirname + '/views');
app.set('view engine', 'jade');
app.use('/public', express.static(__dirname + '/public'));
app.locals.pretty = true;
// load properties from .env file (required by Heroku)
var dotenv = require('dotenv');
dotenv.load();
// load the environment
var PORT = process.env.PORT || 2345;
var API_HOST = process.env.API_HOST || 'localhost';

app.get('/suggestions', function(req, res) {
	api.handleRequest(req,res);
});

app.get('/apitest', function(req, res) {
    res.render('index', { 
    	API_URL: 'http://' + API_HOST + ':' + PORT, 
    	title: 'API endpoint test page', 
    	message: 'API endpoint test page'
    });
});

var server = app.listen(PORT, function() {
	util.log(util.format('Server running at http://%s:%d', API_HOST, PORT));
});
