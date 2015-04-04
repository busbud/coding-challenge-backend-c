// Internal dependencies
var CityTable = require('../database/city-table');
var logger = require('../utils/logger');

// External dependencies
var toobusy = require('toobusy-js');


var middleware = exports;

// Potential middleware
function checkDbState(req, res, next) {

  var isDbUp = CityTable.isDatabaseUp();

  if (!isDbUp) {
    logger
      .warn('Db is not set up...');
    res
      .status(503)
      .send({
        error: 'Service temporary unavailable'
      });
    return;
  }

  next();
}

function checkServerNotBusy(req, res, next) {

  var isBusy = toobusy();

  if (isBusy) {
    logger
      .warn('Server is too busy...');
    res
      .status(503)
      .send({
        error: 'Service temporary unavailable'
      });
    return;
  }

  next();

}

function authorizeRequests(req, res, next) {
  // middleware to use for all requests
  res.header('Access-Control-Allow-Origin', '*');
  res.header('Access-Control-Allow-Methods',
    'GET,PUT,POST,DELETE');
  res.setHeader('Access-Control-Allow-Headers',
    'X-Requested-With,Content-Type, Authorization');

  next();
}

middleware.checkDbState = checkDbState;
middleware.checkServerNotBusy = checkServerNotBusy;
middleware.authorizeRequests = authorizeRequests;
