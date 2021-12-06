var config = require('../config.json');
/*
* Scores a row in the database for exact-match to query
* 
* @async
* @param {object} query parameters
* @param {object} database row
* @returns {Promise} resolves a numeric score of 0 or 1, or null if query string is too short
*/
function getExactMatchScore(query, db) {
  return new Promise(function (resolve, reject) {
    try {
      if (query && query.get('q') && query.get('q').length <= config.limit.query.minLength) {
        resolve(null);
      }
      if (query.get('q').trim().toLowerCase() === db[config.data_columns.name].trim().toLowerCase()) {
        resolve(1);
      }
      else if (query.get('q').trim().toLowerCase() === db[config.data_columns.ascii].trim().toLowerCase()) {
        resolve(1);
      }
      else {
        resolve(0);
      }
    }
    catch (err) {
      reject(err);
    }
  });
};

module.exports = getExactMatchScore;