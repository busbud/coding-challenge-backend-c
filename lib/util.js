'use strict';

var fs = require('fs');
var d3 = require('d3');
var geolib = require('geolib');
var Levenshtein = require('levenshtein');

/**
* Check if object is empty.
* @param {object} obj Object to check.
* @return {boolean}
*/
exports.isEmpty = function(obj){
    return Object.keys(obj).length === 0;
}

/**
* Quick check if arg can be condidered as an explicit latitude 
* @param {number} lat Latitude object.
* @return {boolean}
*/
exports.isLatitude = function(arg){

	if (typeof arg === 'number' && arg <= 90 && arg >= -90){
	    return true;
	} else {
		return false;
	}
}

/**
* Quick check if arg can be condidered as an explicit longitude
* @param {number} lng Longitude object.
* @return {boolean}
*/
exports.isLongitude = function(arg){

	if (typeof arg === 'number' && arg <= 180 && arg >= -180){
		return true;
	} else {
		return false;
	}
}

/**
* Get cities information from the given file
* @param {string} filename Filename.
* @param {function} cb Callback function.
*/
exports.getInfoFromFile = function(filename, cb){

	fs.readFile(filename,'utf8', function(err, data){
		if(err){
			cb(err, null);
		} else {
			var info = d3.tsv.parse(data);
			cb(null, info);
		}
	});
}

/**
* Compute the score according to given geodata.
* The argument should be of this form :
* {
*	'latitude': 38,334,
*	'longitude' : -76,2992
* }
*
* @param {object} arg1 First argument.
* @param {object} arg2 Second argument.
*/
exports.computeScoreGeoloc = function(arg1, arg2){
	var dist = geolib.getDistance(arg1, arg2);
	return dist2Score(dist);
}

/**
* Compute the score by using the Levenshtein fuzzy algorithm.
* @param {string} str String passed in query.
* @param {string} city City name.
*/
exports.computeScoreCity = function(str, city){
	var score = new Levenshtein(str, city);
	score = 1 - score * 0.1;
	return score;

    //Code source from : http://siderite.blogspot.com/2014/11/super-fast-and-accurate-string-distance.html
	//return sift4(str, city, 1, 5);

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

/**
* Pretty print the city name.
* @param {object} city City object.  
*/
exports.prettyPrintCity = function(city){
	switch(city.country){
		case 'US':
			return city.name +', '+ city.admin1 +', USA';
		break;
		case 'CA':
			return city.name +', '+ num2CA(city.admin1) +', Canada';
		break;
		default:
		break;
	}
}

/**
* Convert string rep of number into a Canadian province code.
* @param {string} num String representation of number.  
*/
function num2CA(num){

	switch(num){
		case '01':
			return 'AB';
		break;
		case '02':
			return 'BC';
		break;
		case '03':
			return 'MB';
		break;
		case '04':
			return 'NB';
		break;
		case '05':
			return 'NL';
		break;
		case '06':
			return 'NT';
		break;
		case '07':
			return 'NS';
		break;
		case '08':
			return 'ON';
		break;
		case '09':
			return 'PE';
		break;
		case '10':
			return 'QC';
		break;
		case '11':
			return 'SK';
		break;
		case '12':
			return 'YT';
		break;
		case '13':
			return 'NU';
		break;
		default:
		break;
	}
}

/**
* Convert a distance into a score.
* @param {number} dist Distance(in meters).  
*/
var dist2Score = function(dist) {
	
    if (dist <= 100000) {
    	return 1.0;
    } else if(dist > 100000 && dist <= 200000){
    	return 0.9;
    } else if(dist > 200000 && dist <= 300000) {
    	return 0.8;
    } else if(dist > 300000 && dist <= 400000) {
    	return 0.7;
    } else if(dist > 400000 && dist <= 500000) {
    	return 0.6;
    } else if(dist > 500000 && dist <= 600000){
    	return 0.5;
    } else if(dist > 600000 && dist <= 700000){
    	return 0.4;
    } else if(dist > 700000 && dist <= 800000){
    	return 0.3;
    } else if(dist > 800000 && dist <= 900000){
    	return 0.2;
    } else if(dist > 900000 && dist <= 1000000){
    	return 0.1;
    } else {
    	return 0.0;
    }
}