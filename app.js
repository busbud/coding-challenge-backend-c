// Load config first
const config = require('config');
const {createWriteStream} = require('fs');

const http = require('http');

// Express and its dependencies
const express = require('express');
const compression = require('compression');
const morgan = require('morgan');

// Winston custom logger
const log = require('./lib/logger')('app');

const routesLoader = require('./lib/routesLoader');
const {load} = require('./data/citiesStore.js');

// Make sure one of our 3 valid environment values has been specified
if(!['development', 'production', 'test'].includes(config.env)) {
  log.f(`Invalid environment value specified: ${config.env}`);
}

// Instanciate express
const app = express();

if(config.env === 'production') {
  const accessLogStream = createWriteStream(`${__dirname}/logs/${config.logFilename}`, {flags: 'a'});
  app.use(morgan('short', {stream: accessLogStream}));
} else {
  app.use(morgan('dev', {stream: process.stdout}));
}

app.use(compression());

// Load route files found in routes directory
routesLoader({app, path: `${__dirname}/routes`});

// Handle errors
app.use((err, req, res, next) => {
  if(!err) return next();

  log.e(err);
  res.status(500).json({error: 'Something went wrong'});
});

const server = http.Server(app);

// Handle unhandled promise rejections here, super useful when debugging
process.on('unhandledRejection', e => {
  log.e(e);
});

// Load cities in memory before listening for requests
load(() => {
  server.listen(config.env === 'test' ? 0 : config.port, () => { // Port 0 in test will use a randomly available port
    log.i(`Express server listening on port ${server.address().port} in ${config.env} environment...`);
    app.emit('serverReady');
  });
});

module.exports = app;
