require('dotenv').load();

var express      = require('express'),
    app          = express(),
    logger       = require('morgan');

app.use(logger('dev'));

(require('./lib/suggestions'))(app);

var server = app.listen(process.env.PORT || 2345, function() {
  var host = server.address().address;
  var port = server.address().port;

  console.log('Suggestions api listening at http://%s:%s/suggestions', host, port);
});

module.exports = app;