'use strict';

const logger = require('../libs/logger');
const mongoose = require('mongoose');

// Override mongoose promise with bluebird
mongoose.Promise = require('bluebird');
if (process.env.MONGOOSE_DEBUG) {
  mongoose.set('debug', true);
}

/**
 *  Init DB
 * @param dbUri
 * @param options
 * @param callback
 */
module.exports.initDb = function(dbUri, options, callback) {
  options = options || {};
  callback = callback || function() {
  };
  options.connectTimeoutMS = 30000;
  options.keepAlive = 120;
  options.socketTimeoutMS = 360000;
  options.reconnectTries = Number.MAX_VALUE;
  options.reconnectInterval = 1000;
  options.useNewUrlParser = true;

  mongoose.connect(dbUri, options);

  mongoose.connection.once('open', function() {
    logger.info('DB connected');
    return callback();
  });

  mongoose.connection.on('error', function(err) {
    logger.error('DB connection error: ' + err);
    mongoose.disconnect();
  });

  mongoose.connection.on('disconnected', function() {
    logger.error('DB disconnected!');
    mongoose.connect(dbUri, options);
  });

  mongoose.connection.on('reconnect', function() {
    logger.warn('DB reconnected');
  });
};

/**
 * Close DB
 * @param callback
 * @returns {MongooseThenable|void}
 */
module.exports.closeDb = function(callback) {
  return mongoose.disconnect(callback);
};
