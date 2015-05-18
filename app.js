var express     = require('express'),
    reqHandler  = require('./reqHandler')
    dataBuilder = require('./dataBuilder');

var port = process.env.PORT || 2345;
var app = express();
var db = new dataBuilder();

//set up routes
app.get('/suggestions', reqHandler.getSuggestions)

//catch all
app.use(function(req, res, next) {
    res.set('Connection', 'close')
    res.set('Content-Type', 'text/plain');
    res.status(404).send()
});

//prime the db
db.primeRead().on('primeReadDone', function() {
   app.listen(port, function() {
     console.log('Server running at http://127.0.0.1:%d/suggestions', port);
   });
});

module.exports = {
    app: app,
    db: db
}
