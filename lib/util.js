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
