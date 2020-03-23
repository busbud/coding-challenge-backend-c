var express = require('express');

var suggestionRoute = require('./routes/suggestion_route');
var serverConfig = require('./properties').server;

var app = express();

app.disable('x-powered-by'); // Don't give it away who's serving

app.use('/suggestions', suggestionRoute);

app.listen(serverConfig.port, serverConfig.host);

console.log('Server running at http://%s:%d/suggestions', serverConfig.host, serverConfig.port);

module.exports = app;