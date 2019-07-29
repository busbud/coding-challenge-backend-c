const defaultConfig = require('./config.json');

// ASSUMES THAT WE'RE NOT GOING TO CREATE A SECURE SERVER.
// TODO: ADD NOTES TO README ABOUT IT

module.exports.get = function () {
  return {
    db: {
      url: process.env.DB_URL || defaultConfig.db.url,
      accessKey: process.env.DB_ACCESS_KEY || defaultConfig.db.access_key,
      secret: process.env.DB_SECRET || defaultConfig.db.secret
    },
    log: {
      level: process.env.LOG_LEVEL || defaultConfig.log.level
    },
    server: {
      port: process.env.PORT || defaultConfig.server.port
    },
    data: {
      path: process.env.DATA_PATH || defaultConfig.data.path,
      index: process.env.ES_INDEX || defaultConfig.data.index,
      type: process.env.ES_TYPE || defaultConfig.data.type
    }
  }
};