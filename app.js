var express = require('express');
var morgan = require('morgan');
var compression = require('compression');
var toobusy = require('toobusy');
var _ = require('lodash');
var errorHandler = require('errorhandler');
var mongoose = require('mongoose');

var dbConnection = require('./cfg/mongoose');
var routes = require('./routes/suggestions');

var env = process.env.NODE_ENV || 'development';
var port = process.env.PORT || 2345;

/* setting up the express server */
var app = express();

// middleware add-ons
app.use(morgan('dev'));
app.use(compression());
app.use(function(req, res, next) {
  if (
      // 503 if server overloaded
      // https://www.npmjs.org/package/toobusy
      toobusy()
      ||
      // 503 if database not accessible
      // http://mongoosejs.com/docs/api.html#connection_Connection-readyState
      // https://github.com/LearnBoost/mongoose/blob/master/lib/connectionstate.js
      _.contains([
          mongoose.Connection.STATES.disconnected,
          mongoose.Connection.STATES.connecting,
          mongoose.Connection.STATES.disconnecting,
          mongoose.Connection.STATES.uninitialized], dbConnection.readyState)) {
    res
      .status(503)
      .json({ suggestions: [] });
    return;
  }
  next();
});

app.use(routes);

if (env === 'production') {
  
}
else if (env === 'development') {
  app.use(errorHandler());
}
else {
  throw new Error('Unhandled NODE_ENV value: \'' + env + '\'');
}

// dead-end
app.use(function(req, res, next) {
  res
    .status(404)
    .end();
})


/* when application is shutdown */
process.on('SIGINT', function() {
  console.warn('Server shutdown...');
  dbConnection.close();
  server.close();
  toobusy.shutdown();
  process.exit();
});

// start server
var server = app.listen(port, function() {
  console.log('Server running at http://127.0.0.1:%d/suggestions', port);
});

module.exports = server;
