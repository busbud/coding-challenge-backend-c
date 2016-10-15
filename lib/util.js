var score = require('string-score');
var geolib = require('geolib');

/**
* Compute the matching score by using the string-score module https://www.npmjs.com/package/string-score
* @param {object} city .
* @param {string} str String passed in query string.
*/
exports.getScore = function(city, str, geo){

     var score =  getCityNameMatchingScore(city, str);
     if (score > 0) {
        //If lat and lng are not provide (or incorrect) no need to calculate score
        if (geo.latitude !== undefined && geo.longitude !== undefined
            && isLatitude(geo.latitude) && isLongitude(geo.longitude)) {
          var arg1 = { latitude: geo.latitude, longitude: geo.longitude };
          var arg2 = { latitude: city.latitude, longitude: city.longitude };
          var penalty = getGeolocPenalty(arg1,arg2);
          score =(score-penalty).toFixed(4);
        }
     }
    return score.toFixed(4);
}


/**
* Calculate score penalty according to given geodata.
* The argument should be of this form :
* {
*	'latitude': 38,334,
*	'longitude' : -76,2992
* }
*
* We are using a treshold at 10000km so any distance above this will receive
* the max decrease of 0.5.
* @param {object} arg1 First argument.
* @param {object} arg2 Second argument.
*/
function getGeolocPenalty(arg1, arg2){
  var maxPenalty = 0.5;
  var penalty = 0;
  var dist = geolib.getDistance(arg1, arg2);
  var distance = geolib.convertUnit('km', dist, 2);

  if(distance > 20) {
    penalty = (distance * maxPenalty) / 10000;
    penalty = (penalty > maxPenalty) ? maxPenalty : penalty;
  }
  return penalty;
}

/**
* Compute the matching score by using the string-score module
* https://www.npmjs.com/package/string-score
* @param {object} City.
* @param {string} str String passed in query string.
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

/**
* Dislay city info
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
* Simple sort by score.
* Array in argument must contain object with key named score.
* @param {array} array Array
*/
exports.sortByScore = function(array){
	var sortedArray = array.sort(function(a, b) {
	  return b.score - a.score;
	});
	return sortedArray;
}
