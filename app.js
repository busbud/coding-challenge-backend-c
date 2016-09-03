var http        = require('http');
var port        = process.env.PORT || 2345;
var express     = require('express');
var app         = express();
var mongoose    = require('mongoose');

// Set config for the view rendering
app.engine('html', require('ejs').renderFile);
app.set('view engine', 'html');

// Modules loading
// First, require the router
var suggestionsRouter = require('./routers/suggestions');
var defaultRouter     = require('./routers/default');

// Then, use it
app.use('/suggestions', suggestionsRouter);
app.use('/', defaultRouter);

// Connect to the mongo database
// Once connected , emit the db:connected event
mongoose.connect('mongodb://coding-challenge:busbud@ds019766.mlab.com:19766/heroku_z7p8f5ck', function(error) {

    if(error) {
        app.emit('db:error', error);
        return process.kill(0);
    }

    return app.emit('db:connected');
});

app.on('db:error', function(error) {
    console.log("The server can't be launched because the connection with the mongo database has failed");
    console.log(error.message);
});

// Listen for database connection before launch the server
app.on('db:connected', function() {

    // Finally launch the app
    app.listen(port, '0.0.0.0');

    // And tell to every one
    console.log('Server running at http://0.0.0.1:%d/suggestions', port);
});

// For the tests
module.exports = app;