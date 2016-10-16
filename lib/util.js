var score = require('string-score');
var geolib = require('geolib');

/**
* Get score by using the query string and geodata
* @param {object} city contains a city record
* @param {string} str city name  passed in query string.
* @param {object} geo geolocation data received from the
*/
exports.getScore = function(city, str, geo){
     var score =  getCityNameMatchingScore(city, str).toFixed(4);
     // If nothing match for the name and lat and lng are not provide no need
     // to calculate score based on geo data
     if (score > 0 && geo.latitude !== undefined && geo.longitude !== undefined) {
          var arg1 = { latitude: geo.latitude, longitude: geo.longitude };
          var arg2 = { latitude: city.latitude, longitude: city.longitude };
          var penalty = getGeolocPenalty(arg1,arg2);
          // console.log('penalty ', penalty);
          score =(score-penalty).toFixed(4);
     }
    return score;
}

/**
* Display city info
* @param {object} city City object.
*/
exports.formatCityName = function(city){
	switch(city.country){
		case 'US':
			return city.asciiName +', '+ city.state +', USA';
		break;
		case 'CA':
			return city.asciiName +', '+ city.state  +', Canada';
		break;
		default:
		break;
	}
}

/**
* Get Canadian province code from the geoname admin1 column
* @param {string} num String representation of number.
*/
exports.getCanadianStateCode = function (geonameValue){
  // Mapping from: http://download.geonames.org/export/dump/admin1CodesASCII.txt
  var canadaStateCodes = {
    1: 'AB',
    2: 'BC',
    3: 'MB',
    4: 'NB',
    13: 'NT',
    7: 'NS',
    14: 'NU,',
    8: 'ON',
    9: 'PE',
    10: 'QC',
    11: 'SK',
    12: 'YT',
    5: 'NL'
  };
  return canadaStateCodes[geonameValue];
}

/**
* Simple sort by descending score.
* Array in argument must contain object with key named score.
* @param {array} array Array
*/
exports.sortByScore = function(array){
	var sortedArray = array.sort(function(a, b) {
	  return b.score - a.score;
	});
	return sortedArray;
}

/**
* Calculate score penalty according distance between 2 geolocation point.
* The argument should be of this form :
* {	'latitude': 46.81228, 'longitude' : -71.21454  }
*
* Any distance less than 20 km will receive a 0 penalty
* We are using a treshold at 10000km so any distance above this will receive
* the max penalty of 0.5.
* @param {object} arg1 geo-location point.
* @param {object} arg2 geo-location point.
* @return {number} penalty score
*/
function getGeolocPenalty(arg1, arg2){
  var maxPenalty = 0.5;
  var penalty = 0;

  // Check if geolocation data are valid
  if (isValidGeolocation(arg1) && isValidGeolocation(arg2)) {
    // Calculate distance between 2 points using geol-lib library
    var dist = geolib.getDistance(arg1, arg2);
    var distance = geolib.convertUnit('km', dist, 2);

    // Calculate penalty score if distance is more than 20 km
    if(distance > 20) {
      penalty = (distance * maxPenalty) / 10000;
      penalty = (penalty > maxPenalty) ? maxPenalty : penalty;
    }
  }
  return penalty;
}

/**
* Compute the matching score by using the string-score module
* https://www.npmjs.com/package/string-score
* @param {object} City.
* @param {string} str String passed in query string.
* @return {number} score
*/
function getCityNameMatchingScore(city, str){
   var matchingScore = score(city.name, str);
   //if nothing found with name , check ascii name
   if (matchingScore === 0 ) {
      matchingScore = score(city.asciiName, str);
   }
	return matchingScore;
}

/**
* Check if arg is a valid geolocation point
* The argument should be of this form :
* {	'latitude': 46.81228, 'longitude' : -71.21454  }
* @param {object} geolocation value, must contains latitude and longitude
* @return {boolean}
*/
function isValidGeolocation(arg){
   return isLatitude(arg.latitude) && isLongitude(arg.longitude);
}

/**
* Check if arg is an valid latitude
* @param {number} latitude value.
* @return {boolean}
*/
function isLatitude(arg){
	if (typeof arg === 'number' && arg <= 90 && arg >= -90){
	    return true;
	}
  return false;
}

/**
* Check if arg is an valid longitude
* @param {number} longitude value.
* @return {boolean}
*/
function isLongitude(arg){
	if (typeof arg === 'number' && arg <= 180 && arg >= -180){
		return true;
	}
	return false;
}
