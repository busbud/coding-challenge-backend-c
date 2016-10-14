var score = require('string-score');

/**
* Compute the matching score by using the string-score module https://www.npmjs.com/package/string-score
* @param {string} city City name.
* @param {string} str String passed in query string.
*/
exports.getCityNameMatchingScore = function(city, str){
	return score(city, str);
}

/**
* Dislay city info
* @param {object} city City object.
*/
exports.cityDisplayInfo = function(city){
	switch(city.country){
		case 'US':
			return city.name +', '+ city.state +', USA';
		break;
		case 'CA':
			return city.name +', '+ city.state  +', Canada';
		break;
		default:
		break;
	}
}

/**
* Get Canadian province code from the geoname admin1 column
* @param {string} num String representation of number.
*/
exports.getCanadianProvinceIsoCode = function (geonameValue){
  var provinces = {
  	1:'AB',
  	2:'BC',
  	3:'MB',
  	4:'NB',
  	5:'NL',
  	6:'',
  	7:'NS',
  	8:'ON',
  	9:'PE',
  	10:'QC',
  	11:'SK',
  	12:'YT',
  	13:'NT',
  	14:'NU'
  }
  return provinces[geonameValue];
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
