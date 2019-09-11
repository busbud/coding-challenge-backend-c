const request = require('request');

/**
 * Validates data for a given region returned by Geodata service.
 * Else returns an empty object
 * @param {Object} JSON object contaning data for a given region.
 * @param {Boolean} indicates whether we need to validate population data for the region.
 * @param {string} region prefix supplied by the user.
 * @return {Object} Returns region object post validation or a null object if validation fails.
 */
function validateAndGetCityDataFromJsonFields(jsonFields, checkPopulation, regionPrefix) {
  var obj;
  // Validating all required fields are present in the json before returning data object
  if (
    ('name' in jsonFields) && (jsonFields.name) && (jsonFields.name.length > 0)
    && jsonFields.name.includes(regionPrefix)
    && ('adminCodes1' in jsonFields) && ('ISO3166_2' in jsonFields.adminCodes1)
    && (jsonFields.adminCodes1.ISO3166_2) && (jsonFields.adminCodes1.ISO3166_2.length > 0)
    && ('countryCode' in jsonFields) && (jsonFields.countryCode) && (jsonFields.countryCode.length > 0)
    && ('lat' in jsonFields) && !Number.isNaN(jsonFields.lat)
    && ('lng' in jsonFields) && !Number.isNaN(jsonFields.lng)
    && ((!checkPopulation) || (checkPopulation
      && ('population' in jsonFields) && !Number.isNaN(jsonFields.population)
      && jsonFields.population > 0))
  ) {
    obj = {};
    obj.name = jsonFields.name + ', ' + jsonFields.adminCodes1.ISO3166_2 + ', ' + jsonFields.countryCode;
    obj.latitude = jsonFields.lat;
    obj.longitude = jsonFields.lng;

    return obj;
  }
  return null;
}

/**
 * Retrieves scores for various regions either using Euclidean distance from user's location
 * or population as a metric to determine scores.
 * @param {Object} JSON object contaning data about various regions.
 * @param {number} user specified lattitude.
 * @param {number} user specified longitude.
 * @param {string} region prefix supplied by the user.
 * @return {Object} JSON object contaning regions starting with regionPrefix & their scores.
 */
function getScoresForCityData(jsonObject, latitude, longitude, regionPrefix) {
  var maxScore = Number.MIN_VALUE;
  var minScore = 0;
  var obj;
  var i;

  var returnJsonObj = {
    suggestions: [],
  };

  // Flag to indicate if we are using proximity to user's location to determine the score
  var areUserCoordinatesSpecified = true;
  if (latitude == null && longitude == null) {
    areUserCoordinatesSpecified = false;
  }

  for (i = 0; i < jsonObject.geonames.length; i += 1) {
    // Validate and return object if all the concerned fields in the json seem valid
    obj = validateAndGetCityDataFromJsonFields(jsonObject.geonames[i],
      !areUserCoordinatesSpecified, regionPrefix);

    if (obj != null) {
      if (areUserCoordinatesSpecified) {
        // Score is tentatively assigned to Euclidean distance from user's current location
        // obj.score = Math.pow(Math.pow(obj.longitude - longitude, 2)
        // + Math.pow(obj.latitude - latitude, 2), 0.5);
        obj.score = (((obj.longitude - longitude) ** 2) + ((obj.latitude - latitude) ** 2)) ** 0.5;
      } else {
        // Score is tentatively assigned to the regionPrefix's log(population)
        obj.score = Math.log10(jsonObject.geonames[i].population);
      }
      maxScore = Math.max(maxScore, obj.score);
      returnJsonObj.suggestions.push(obj);
    } else {
      // console.log('Encountered invalid geoname entry for region prefix ' + regionPrefix + '.');
    }
  }

  /*
  To avoid cases where maxScore = minScore.
  It can happen when we have only 1 element in the array with min score
  */
  maxScore += 1;

  for (i = 0; i < returnJsonObj.suggestions.length; i += 1) {
    if (areUserCoordinatesSpecified) {
      /*
      Normalize score between 0 & 1
      Substract the score from 1 to give lower Euclidean distances a higher score
      */
      returnJsonObj.suggestions[i].score = (1.0
        - (returnJsonObj.suggestions[i].score - minScore) / (maxScore - minScore)).toFixed(2);
    } else {
      // Normalize population score between 0 and 1
      returnJsonObj.suggestions[i].score = ((returnJsonObj.suggestions[i].score - minScore)
        / (maxScore - minScore)).toFixed(2);
    }
  }

  // Sort the cities in descending order of scores
  returnJsonObj.suggestions.sort(function sortByDescendingScores(a, b) {
    return b.score - a.score;
  });

  return returnJsonObj;
}

/**
 * Retrieves various regions with a given region prefix from Geonames API.
 * API reference http://www.geonames.org/export/geonames-search.html
 * @param {string} region prefix supplied by the user.
 * @param {Function} callback with the results when finished.
 */
function getCityDataFromGeoNames(regionPrefix, callback) {
  var reqUri = 'http://api.geonames.org/searchJSON?';
  var resultJsonObj;
  const userName = 'soyboyxvx702';
  // var options;
  reqUri += 'username=' + userName;
  reqUri += '&country=US';
  reqUri += '&country=CA';
  reqUri += '&cities=cities5000';
  reqUri += '&fields=name,countryCode,adminCodes1,lng,lat,population';
  reqUri += '&name_startsWith=' + regionPrefix;
  reqUri += '&maxRows=1000';

  /* options = {
    url: reqUri,
    headers: {
      'X-Busbud-Token': 'PARTNER_AHm3M6clSAOoyJg4KyCg7w',
    },
  }; */

  // console.log('Making the following HTTP GET request to geoname: ' + reqUri);

  request(reqUri, function callbackHandlerGeoNameResponse(error, response, body) {
    if (!error && response.statusCode == 200) {
      // console.log('Geoname response for region: ' + regionPrefix + ' is: ' + body);
      resultJsonObj = JSON.parse(body);
      return callback(null, resultJsonObj);
    }
    if (!error) {
      return callback(
        new Error('Unexpected error while fetching data, response status code ' + response.statusCode), null
        );
    }
    return callback(error, null);
  });
}

/**
 * Retrieves various regions with a given region prefix along with scores assigned to those regions.
 * @param {string} region prefix supplied by the user.
 * @param {number} user specified lattitude.
 * @param {number} user specified longitude.
 * @param {Function} callback with the results when finished.
 */
function getRegionalDataAndCalculateScores(regionPrefix, latitude,
  longitude, callbackToReturnResult) {
  getCityDataFromGeoNames(regionPrefix, function processGeoNameData(err, data) {
    var resultJsonObject;
    var responseStatusCode;
    if (!err) {
      if (data && ('geonames' in data)) {
        resultJsonObject = getScoresForCityData(data, latitude, longitude, regionPrefix);
        if (resultJsonObject.suggestions.length > 0) {
          responseStatusCode = 200;
        } else {
          responseStatusCode = 404;
        }
        callbackToReturnResult(null, JSON.stringify(resultJsonObject), responseStatusCode);
      } else {
        callbackToReturnResult(new Error('Unexpected error while fetching data'), null, 500);
      }
    } else {
      callbackToReturnResult(err, null, 500);
    }
  });
}

exports.getCityDataWithScore = getRegionalDataAndCalculateScores;
