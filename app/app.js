var express = require('express');
var suggestionRoute = require('./routes/suggestion_route')

var host = process.env.HOST || '0.0.0.0';
var port = process.env.PORT || 8080;
var app = express();

app.use('/suggestions', suggestionRoute);

app.listen(port, host);

console.log('Server running at http://%s:%d/suggestions', host, port);

module.exports = app;