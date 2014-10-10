var express = require('express');
var util = require('util');
var app = express();
// all environments
app.set('port', process.env.PORT || 3000);
app.set('views', __dirname + '/views');
app.set('view engine', 'jade');
app.use('/public', express.static(__dirname + '/public'));
app.locals.pretty = true;
// first load properties from .env file
var dotenv = require('dotenv');
dotenv.load();
// then load the environment
var API_PORT = process.env.PORT || 2345;
var API_HOST = process.env.API_HOST || 'localhost';

app.get('/', function(req, res) {
    res.render('index', { 
    	API_URL: 'http://' + API_HOST + ':' + API_PORT, 
    	title: 'API endpoint test page', 
    	message: 'API endpoint test page'
    });
});

var server = app.listen(3000, function() {
    util.log(util.format('Listening on port %d', server.address().port));
});
