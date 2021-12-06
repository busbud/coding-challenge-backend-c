var reverse = require('reverse-geocode');
var config = require('../config.json');

/*
* Scores a row in the database based on if query lat/long is same state as db row
* 
* @async
* @param {object} query params
* @param {object} database row
* @returns {Promise} resolves 1 if state matches, 0.5 if only region matches, 0 if no match, and null if coordinates not provided
*/
function getStateScore (query, db) {
  return new Promise(function (resolve, reject) {
    try {
      if (!query.get('latitude') || !query.get('longitude')) {
        resolve(null)
      }
      var queryState;
      var queryRegion;
      var dbState;
      var dbRegion;
      var score = 0;
      var countries = config.limit.country.include;
      for (var i = 0; i < countries.length; i++) {
        var queryLookup = reverse.lookup(parseFloat(query.get('latitude')), parseFloat(query.get('longitude')), countries[i]);
        var dbLookup = reverse.lookup(parseFloat(db[config.data_columns.lat]), parseFloat(db[config.data_columns.long]), countries[i]);
        if (queryLookup) {
          queryState = queryLookup.state_abbr;
          queryRegion = queryLookup.region;
        }
        if (dbLookup) {
          dbState = dbLookup.state_abbr;
          dbRegion = dbLookup.region;
        }
        if (dbState && queryState && dbState === queryState) {
          score++
          break;
        }
      }
      if (score === 0 && dbRegion && queryRegion && dbRegion === queryRegion) {
        score = 0.5;
      }
      resolve(score);
    }
    catch (err) {
      reject(err);
    }
  })
}

module.exports = getStateScore;