var config = require('../config.json');
/*
* Scores a row in the database for near-match to query
* 
* @async
* @param {object} query parameters
* @param {object} database row
* @returns {Promise} resolves a numeric score between 0 and 1, or null if query string is too short
*/
function getNearMatchScore(query, db) {
  return new Promise(function (resolve, reject) {
    try {
      if (query && query.get('q') && query.get('q').length <= config.limit.query.minLength) {
        resolve(null);
      }
      var queryString = query.get('q').trim().toLowerCase();
      var dbAsciiName = db[config.data_columns.ascii].trim().toLowerCase();
      if (dbAsciiName.indexOf(queryString) !== -1) {
        var percentMatch = queryString.length / dbAsciiName.length;
        var percentIndex = 1 - (dbAsciiName.indexOf(queryString) / dbAsciiName.length);
        resolve((percentMatch + percentIndex) / 2);
      }
      else if (queryString.indexOf(dbAsciiName) !== -1) {
        var percentMatch = dbAsciiName.length / queryString.length;
        var percentIndex = 1 - (queryString.indexOf(dbAsciiName) / queryString.length);
        resolve((percentMatch + percentIndex) / 2);
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

module.exports = getNearMatchScore;