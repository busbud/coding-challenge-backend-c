// External Dependencies
var express = require('express');
var path = require('path');
var Q = require('q');
var toobusy = require('toobusy-js');

// Internal Dependencies
var logger = require('./utils/logger');


// Setup
var port = process.env.PORT || 5000; // set our port
//toobusy.maxLag(50);


// Create express app
var app = express();
app.set('port', port);


// Database
var dbCity = require('./database/city-table');


// Middleware
var middleware = require('./middleware/middleware')


// Controllers
var suggestionsController = require('./controllers/suggestions-controller');
var mileageController = require('./controllers/mileage-controller');


// Use middleware functions for request parsing
// for all url begining with '/'
app
  .use('/',
    middleware.authorizeRequests,
    middleware.checkServerNotBusy);

// The database has to be set up
app
  .use('/suggestions',
    middleware.checkDbState);



// Register routes and methods
app
  .get('/suggestions', suggestionsController.suggestByName);

app
  .get('/mileage', mileageController.computeDistance);



// Launch the server
var server = app.listen(app.get('port'), function () {

  // Populate db with data
  Q.fcall(function () {
    var srcFilePath = path.join(__dirname,
      '/data/cities_canada-usa.tsv');
    var destFilePath = path.join(__dirname,
      '/data/cities_canada-usa.json');
    dbCity.populateDatabase(srcFilePath, destFilePath);
  });

  logger.info('Server listening at port %s', port);

});


// Show uncaught exception
process
  .on('uncaughtException', function (err) {
    logger.error('Uncaught exception : ', err);
  });

process
  .on('SIGINT', function () {
    server.close();
    toobusy.shutdown();
    process.exit();
  });


module.exports = app;
