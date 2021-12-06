
var config = require('../config.json');
/*
* Uses the ‘haversine’ formula to calculate the great-circle distance between two points 
* 
* @async
* @param {number} lat of query
* @param {number} long of query
* @param {number} lat of database row
* @param {number} long of database row
* @returns {number} kilometers between query coordinates and database row coordinates
* @link https://www.movable-type.co.uk/scripts/latlong.html
*/
function getKilometers(lat1, lon1, lat2, lon2) {
  var earthRadius = 6371e3; // metres
  var latOneRadians = lat1 * Math.PI/180; // φ, λ in radians
  var latTwoRadians = lat2 * Math.PI/180;
  var latRadians = (lat2-lat1) * Math.PI/180;
  var longRadians = (lon2-lon1) * Math.PI/180;

  var a = Math.sin(latRadians/2) * Math.sin(latRadians/2) +
            Math.cos(latOneRadians) * Math.cos(latTwoRadians) *
            Math.sin(longRadians/2) * Math.sin(longRadians/2);
  var c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1-a));

  var d = earthRadius * c; // in metres
  return d / 1000;
}

/*
* Scores a row in the database for distance to query
* 
* @async
* @param {object} query parameters
* @param {object} database row
* @returns {Promise} resolves a numeric score between 0 and 1 or null if coordinates not provided
*/
function getDistanceScore (query, db) {
  return new Promise(function(resolve, reject) {
    try {
      if (!query.get('latitude') || !query.get('longitude')) {
        resolve(null)
      }
      var halfEarthCircumference = 20037.5;
      var distance = getKilometers(+query.get('latitude'), +query.get('longitude'), +db[config.data_columns.lat], +db[config.data_columns.long]);
      resolve(1 - (distance / halfEarthCircumference));
    }
    catch (err) {
      reject(err);
    }
  });
};

module.exports = getDistanceScore;