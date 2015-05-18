var express    = require('express'),
    reqHandler = require('./reqHandler');

var port = process.env.PORT || 2345;
var app = express();

app.get('/suggestions', reqHandler.getSuggestions)

app.use(function(req, res, next) {
    res.set('Connection', 'close')
    res.set('Content-Type', 'text/plain');
    res.status(404).send()
});

var server = app.listen(port, function() {
   console.log('Server running at http://127.0.0.1:%d/suggestions', port);
});

module.exports = app