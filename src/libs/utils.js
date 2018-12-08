'use strict';

const fs = require('fs');
const logger = require('./logger');
const GeoNames = require(_src + '/models').geonames;

/**
 * Error Handler
 * @param error
 */
module.exports.onError = function(error) {
  if (error.syscall !== 'listen') {
    throw error;
  }

  const bind = typeof process.env.PORT === 'string' ? 'Pipe ' + process.env.PORT : 'Port ' + process.env.PORT;
  // handle specific listen errors with friendly messages
  switch (error.code) {
    case 'EACCES':
      logger.error(bind + ' requires elevated privileges');
      process.exit(1);
      break;
    case 'EADDRINUSE':
      logger.error(bind + ' is already in use');
      process.exit(1);
      break;
    default:
      logger.error('UncaughtException', error);
      throw error;
  }
};

/**
 * TSV to JSON
 * @param filePath
 * @returns {Array}
 * @private
 */
function _tsvToJson(filePath) {
  const tsv = fs.readFileSync(filePath, 'utf8'); // => <tsv data>
  const lines = tsv.split('\n');
  const result = [];
  const headers = lines[0].split('\t');
  for (let i = 1; i < lines.length; i++) {
    const obj = {};
    const thisRow = lines[i].split('\t');
    for (let j = 0; j < headers.length; j++) {
      obj[headers[j]] = thisRow[j];
    }
    if (!obj.long || !obj.lat) {
      obj.geoPosition = [0, 0];
    } else {
      obj.geoPosition = [parseFloat(obj.long.replace(',', '.')), parseFloat(obj.lat.replace(',', '.'))];
    }

    result.push(obj);
  }
  return result;
}

/**
 * Populate DB
 * @returns {Promise<void>}
 */
module.exports.populateDB = async function() {
  const data = await GeoNames.findOne({alt_name: 'LOZ'});
  if (!data._id) {
    const data = _tsvToJson(_base + '/data/test.tsv');
    return await GeoNames.insertMany(data);
  }
};
