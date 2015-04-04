// Internal dependencies
var CityTable = require('../database/city-table');

// External dependencies
var Q = require('q');


var suggestionsRepository = exports;


function getCityBeginingWith(name) {

  // There, we trust the given param
  var deferred = Q.defer();

  var isDbUp = CityTable.isDatabaseUp();
  if (!isDbUp) {
    deferred.reject(new Error('The database is not up right now'));
    return deferred.promise;
  }

  CityTable
    .findByPrefix(name, function(err, data) {
      if (err) {
        deferred.reject(err);
        return;
      }
      deferred.resolve(data);
    });

  return deferred.promise;

}


suggestionsRepository.getCityBeginingWith = getCityBeginingWith;
