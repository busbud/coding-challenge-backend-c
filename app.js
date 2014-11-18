var express = require('express');
var morgan = require('morgan');
var errorHandler = require('errorhandler');

var mongoose = require('./cfg/mongoose');
var routes = require('./routes/suggestions');

var port = process.env.PORT || 2345;

// setting up the express server
var app = express();

// middleware add-ons
app.use(morgan('dev'));

app.use(routes);

if (process.env.NODE_ENV !== 'production') {
  app.use(errorHandler());
}

// start server
var server = app.listen(port, function() {
  console.log('Server running at http://127.0.0.1:%d/suggestions', port);
});

module.exports = server;
