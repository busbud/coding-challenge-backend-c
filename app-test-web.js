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
var PORT = process.env.PORT || 2345;

app.get('/', function(req, res){
    res.render('index', { 
    	API_URL: 'http://localhost:' + PORT, 
    	title: 'API endpoint test page', 
    	message: 'API endpoint test page'
    });
});

var server = app.listen(3000, function() {
    util.log('Listening on port %d', server.address().port);
});
