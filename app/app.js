var express = require('express');

var suggestionRoute = require('./routes/suggestion_route');
var serverConfig = require('./properties').server;

var errorHandler = require("./error_handler");

var app = express();

app.use('/suggestions', suggestionRoute);

app.listen(serverConfig.port, serverConfig.host);

console.log('Server running at http://%s:%d/suggestions', serverConfig.host, serverConfig.port);

module.exports = app;