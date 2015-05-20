var express     = require('express'),
    dbBuilder = require('./dbBuilder'),
    reqHandler  = require('./reqHandler');

var port = process.env.PORT || 2345;
var app  = express();
var db   = new dbBuilder();
var rq   = new reqHandler();

//set up routes
app.get('/suggestions', rq.getSuggestions())

//catch all
app.use(function(req, res, next) {
    res.set('Connection', 'close')
    res.set('Content-Type', 'text/plain');
    res.status(404).send()
});

//prime the db
db.primeRead().on('primeReadDone', function() {
    rq.db = db;

    app.listen(port, function() {
        console.log('Server running at http://127.0.0.1:%d/suggestions', port);
    });
});

module.exports = {
    app: app,
    db: db
}