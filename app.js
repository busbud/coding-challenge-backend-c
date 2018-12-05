'use strict';

const path = require('path');
process.env.NODE_ENV = process.env.NODE_ENV || 'development';

// global path
global._base = __dirname;

// src path
global._src = path.join(__dirname, '/src');

// Set environment variable
if (!process.env.AWS) {
  require('dotenv').config();
}

const _ = require('lodash');
const logger = require('./src/libs/logger');
const http = require('http');
const express = require('express');
const bodyParser = require('body-parser');
const cors = require('cors');
const app = express();
const utils = require('./src/libs/utils');
const compression = require('compression');
const helmet = require('helmet');
const cluster = require('cluster');
const db = require('./src/services/db');
const session = require('express-session');

// init databases
// init databases
if (process.env.MONGODB_USER && process.env.MONGODB_PASS) {
  // case of mlab with credential (user + pass)
  db.initDb(process.env.MONGODB_URI, {
    pass: process.env.MONGODB_PASS,
    user: process.env.MONGODB_USER
  });
} else {
  db.initDb(process.env.MONGODB_URI);
}

// Format logs
if (app.get('env') === 'development') {
  app.use(function(req, res, next) {
    logger.debug('%s %s:', _.toUpper(req.method), req.originalUrl);
    logger.debug('- Headers:', JSON.stringify(req.headers));
    logger.debug('- Params:', JSON.stringify(req.params));
    logger.debug('- Query:', JSON.stringify(req.query));
    logger.debug('- Body:', JSON.stringify(req.body));
    return next();
  });
}

// Session
app.use(session({
  resave: false,
  saveUninitialized: false,
  secret: process.env.SESSION
}));

// CORS: same origin policy
app.use(cors());

// Parse json requests' body
app.use(bodyParser.json());

// security
app.use(helmet());

// Parse urlencoded requests' body
app.use(bodyParser.urlencoded({
  extended: true
}));

// Router
app.use(require('./src/router'));

// Create server
let server;
if (app.get('env') === 'production' && cluster.isMaster) {
  const numCPUs = require('os').cpus().length;

  for (let i = 0; i < numCPUs; i++) {
    cluster.fork(); // create a worker
  }

  cluster.on('online', function(worker) {
    logger.info('Worker ' + worker.process.pid + ' is online.');
  });
  cluster.on('exit', function(worker) {
    logger.info('worker ' + worker.process.pid + ' died.');
  });
} else {
  server = http.createServer(app).listen(process.env.PORT, function() {
    logger.info('Launching Search API on port ' + process.env.PORT + ' in ' + app.get('env') + ' mode');
  });
}

// Compression
app.use(compression());

/**
 * Graceful shutdown
 * @returns {*|void}
 * @private
 */
function _shutdown() {
  logger.info('App termination...');
  return server.close(function() {
    logger.info('http server closed');
    logger.info('App terminated');
    return process.exit(0);
  });
}

process.on('SIGINT', _shutdown);
process.on('SIGTERM', _shutdown);

process.on('uncaughtException', utils.onError);

module.exports = app;
