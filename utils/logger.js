// External Dependencies
var winston = require('winston');

// Internal Dependencies
var _ = require('./underscore_with_string');


var winstonConfig = {
  "consoleLogLevel": "debug"
};


// custom levels and colors
var logLevels = {
  levels: {
    'sharp': 0,
    'trace': 1,
    'verbose': 2,
    'debug': 3,
    'validation': 4,
    'info': 5,
    'warn': 6,
    'error': 7
  },

  colors: {
    'sharp': 'white',
    'trace': 'white',
    'verbose': 'white',
    'debug': 'blue',
    'validation': 'blue',
    'info': 'green',
    'warn': 'yellow',
    'error': 'red'
  }
};


var consoleTransport = new(winston.transports.Console)({
  'colorize': true,
  'level': winstonConfig.consoleLogLevel,
  'timestamp': true
});


// Transports
var transports = [consoleTransport];


// Create the logger
var logger = new(winston.Logger)({
  transports: transports
});


logger.setLevels(logLevels.levels);
winston.addColors(logLevels.colors);



module.exports = logger;
