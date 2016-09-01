var http        = require('http');
var port        = process.env.PORT || 2345;
var express     = require('express');
var app         = express();

// Modules loading
// First, require the router
var suggestionsRouter = require('./modules/suggestions/router');

// Then, use it
app.use('/suggestions', suggestionsRouter);

// Finally launch the app
app.listen(port, '127.0.0.1');

// And tell to every one
console.log('Server running at http://127.0.0.1:%d/suggestions', port);

// For the tests
module.exports = app;