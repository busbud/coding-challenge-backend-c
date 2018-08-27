// log4js-wrapper is a module I created.
// I was using log4js in my projects I always ended up overriding a lot of stuff to suit my preferences.
// I decided to make it a module so I can quickly import it
// Formatting for different cases is well handles, it shows the dile and line number of the log when not in production mode and log levels are shorter.
const Logger = require('log4js-wrapper');
const config = require('config');

module.exports = (alias, force) => {
  const log = new Logger(config.log.level, alias, config.env === 'production' ? `${__dirname}/../logs/${config.log.file}` : null);
  if(force) log.setForceAlias(force);
  return log;
};
