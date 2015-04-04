// Internal dependencies
var scoringService = require('./scoring-service');
var suggestionsRepository = require('../repositories/suggestions-repository');
var _ = require('../utils/underscore_with_string');
var logger = require('../utils/logger');
var Constant = require('../config/constant');

// External dependencies
var Q = require('q');


var suggestionsService = exports;


function suggestCityForOptions(opts) {

  return Q.fcall(function() {

    // Check if opts is an object
    var isObjectParam = _.isObject(opts);
    if (!isObjectParam) {
      throw new Error('SuggestionService#suggestByName -- ' +
        'parameter is not an object')
    }

    // Check if there is a name given by the controller
    var hasParamName = _.has(opts, 'name');
    if (!hasParamName) {
      throw new Error('SuggestionService#suggestByName -- ' +
        'No param named \'name\'');
    }

    // Check if there is latitude and longitude given
    // It is required two have both to true or both to false
    var hasParamLatitude = _.has(opts, 'latitude');
    var hasParamLongitude = _.has(opts, 'longitude');
    var canComputeScoring = (hasParamLatitude === hasParamLongitude);
    if (!canComputeScoring) {
      throw new Error('SuggestionService#suggestByName -- ' +
        'Needs longitude AND latitude to work well');
    }

    // Create local vars
    var name = opts.name;
    var latitude = !!hasParamLatitude ? opts.latitude : null;
    var longitude = !!hasParamLongitude ? opts.longitude : null;
    var scoreOpts = {
      name: name,
      latitude: latitude,
      longitude: longitude
    };


    // Check if given name is not blank
    if (_.str.isBlank(name)) {
      throw new Error('SuggestionService#suggestByName -- ' +
        'Can not query if there is no name to search');
    }

    var shouldComputeScoring = (latitude !== null && longitude != null);

    return suggestionsRepository
      .getCityBeginingWith(name)
      .then(
        // Promise succeed
        function(cities) {

          // Build dtos
          var cityDtos = buildCityDtos(cities, scoreOpts);
          var sortedCitiesByScore = _.sortBy(cityDtos, function(dto) {
            return dto.score;
          });

          var reversedResultArray = sortedCitiesByScore.reverse();

          // Just return the X more pertinent values
          var maxValues = Constant.LIMIT_RESPONSE_SIZE;
          var slicedResult = reversedResultArray.slice(0, maxValues);

          // Build the response
          var responseValues = {
            suggestions: slicedResult
          };

          // Return the response
          return responseValues;

        },
        // Promise rejected
        function(err) {
          var errorMessage = [];
          errorMessage.push('SuggestionService#suggestByName --[Error]');
          errorMessage.push(err.message);
          logger.warn(errorMessage.join(' '));
          throw err;
        });
  });

}



// Giving an array of db object return an array of dto
// We also need to provide the opts of the user
function buildCityDtos(dbMatches, opts) {

  var isDbMatchAnArray = _.isArray(dbMatches);
  if (!isDbMatchAnArray) {
    throw new Error('SuggestionService#buildCityDtos -- ' +
      'No array given to build dto');
  }

  // Early return
  if (_.isEmpty(dbMatches)) {
    return [];
  }

  // Array of result dto
  var cityDtos = [];

  _.each(dbMatches, function(dbCity) {

    // Build the dto for the current city result
    var cityDto = buildCityDto(dbCity, opts);
    cityDtos.push(cityDto);

  });

  return cityDtos;

}


// Giving a db object return dto
function buildCityDto(dbCity, opts) {

  // New object
  var cityDto = {
    name: null,
    latitude: null,
    longitude: null,
    score: null
  };

  // Initial Score
  var scoreName = 1.0;
  var scoreCoordinate = 1.0;

  // Build name and score with the name
  var hasCityName = _.has(dbCity, 'name');
  if (hasCityName && !!dbCity.name) {

    var nameParts = [];
    var dbName = dbCity.name;
    nameParts.push(dbName);

    var hasFeatCode = _.has(dbCity, 'feat_code');
    if (hasFeatCode && !!dbCity.feat_code) {
      nameParts.push(dbCity.feat_code);
    }

    var hasCountry = _.has(dbCity, 'country');
    if (hasCountry && !!dbCity.country) {
      nameParts.push(dbCity.country);
    }

    cityDto.name = nameParts.join(', ');

    // Compute name scoring
    var paramName = opts.name;
    scoreName = scoringService.computeScoreForString(dbName, paramName);

  }

  // Build latitude, longitude and score with them
  // if they are set
  var hasCityLatitude = _.has(dbCity, 'lat');
  if (hasCityLatitude && !!dbCity.lat) {

    var dbLat = dbCity.lat;
    cityDto.latitude = dbLat;

    var hasCityLongitude = _.has(dbCity, 'long');
    if (hasCityLongitude && !!dbCity.long) {

      var dbLong = dbCity.long;
      cityDto.longitude = dbLong;


      // Compute coordinate scoring only if we are asking for
      // (with lat and long params)

      var paramLat = opts.latitude;
      var paramLong = opts.longitude;

      var wantedParams = {
        latitude: paramLat,
        longitude: paramLong
      };

      var foundParams = {
        latitude: dbLat,
        longitude: dbLong
      };

      if (!!paramLong && !!paramLat) {
        scoreCoordinate = scoringService.computeScoreForCoordinates(foundParams, wantedParams);
      }
    }
  }

  // Final score is a multiplication with equal ponderation
  // between the name score and the (lat, long) score
  var finalScore = scoreName * scoreCoordinate;
  cityDto.score = finalScore;

  // Return the DTO
  return cityDto;

}



// Exposed functions
suggestionsService.suggestCityForOptions = suggestCityForOptions;
