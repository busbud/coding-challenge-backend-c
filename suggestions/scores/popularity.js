var config = require('../config.json');
var rs = require('rocket-store');
rs.options({
  data_storage_area: __dirname + '/cache-db',
  data_format: rs._FORMAT_JSON
});

/*
* Scores a row in the database based on how many times the frequency the row was previously suggested
* 
* @async
* @param {object} query term
* @param {object} database row
* @returns {Promise} resolves a number between 1 and 0 representing the popularity score
*/
function getPopularityScore (query, db) {
  return new Promise(function(resolve, reject) {
    try {
      var matchCount;
      var totalCount;
      rs.get('suggestions', db[config.data_columns.ascii].trim().toLowerCase())
      .then(function (results) {
        if (results.count >= 1) {
          matchCount = results.count;
        }
        else {
          resolve(0);
        }
        return rs.get('totals', 'matchCount');
      })
      .then(function(results) {
        if (results.result[0].count >= 1) {
          totalCount = results.result[0].count;
          resolve(matchCount / totalCount);
        }
        else {
          resolve(0);
        }
      })
      .catch(function (err) {
        reject(err);
      })
    }
    catch (err) {
      reject(err);
    }
  })
}

module.exports = getPopularityScore;