var util = require('util');
var geolib = require('geolib'); 
var dotenv = require('dotenv').load();
var DEBUG_MODE = process.env.DEBUG_MODE || 'false';

String.prototype.startsWith = function startsWith(pattern) {
	return this.slice(0, pattern.length) == pattern;
}

function log(str, p1, p2) {
	if (DEBUG_MODE == 'true') {
		if (p2 != undefined) {
			util.log(util.format(str,p1,p2));
		} else if (p1 != undefined) {
			util.log(util.format(str,p1));
		} else {
			util.log(util.format(str));
		}
	}
}

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

function getDisplayName(city) {
	if (city.country == 'US') {
		return city.ascii + ', ' + city.admin1 + ', USA';
	} else {
		return city.name + ', ' + provinces[city.admin1] + ', Canada';
	}
}

var testCities = {
	'montreal': {'latitude':'45.50884', 'longitude':'-73.58781'},
	'new york': {'latitude':'40.71427', 'longitude':'-74.00597'},
	'los angeles': {'latitude':'34.05223', 'longitude':'-118.24368'},
	'vancouver': {'latitude':'49.24966', 'longitude':'-123.11934'},
	'miami': {'latitude':'25.77427', 'longitude':'-80.19366'}
}

function getScore(distance) {
	var score = 0;
	var maxCircumference = 20039; // max kilometers along equator
	score = distance/maxCircumference;
	// closer results will have lowest difference, i.e. closer to 0, but the score = a confidence
	// level where 1 not 0 is high, so we need to reverse the figure by subtracting it from 1
	return (1-score);
}

function getDistance(parts, lat2, long2) {
	// handle geo-location convenience arguments for testing
	var city = parts.query['city'];
	if (city != undefined && city.trim() != '' && testCities[city.toLowerCase()]) {
		myLat = testCities[city.toLowerCase()]['latitude'];
		myLong = testCities[city.toLowerCase()]['longitude'];
	} else {
		myLat = parts.query['latitude'];
		myLong = parts.query['longitude'];
	}
	// only compute distance if we were passed in valid numeric arguments
	if (myLat == undefined || myLat.trim() == '' || 
		myLong == undefined || myLong.trim() == '' ||
		isNumeric(myLat) == false || isNumeric(myLong) == false) {
		return NaN;
	} else {
		var distance = geolib.getDistance(
		    {latitude: myLat, longitude: myLong}, 
		    {latitude: lat2, longitude: long2}
		)/1000; // divide by 1000 to convert meters to kilometers
		return Math.floor(distance);
	}
}

function compare(cityA,cityB) {
	if (cityA.score) {
		if (cityA.score < cityB.score)
			return 1;
		if (cityA.score > cityB.score)
			return -1;
	// sort alphabetically if there is no score
	} else {
		if (cityA.name > cityB.name)
			return 1;
		if (cityA.name < cityB.name)
			return -1;
	}
}

function isNumeric(obj) {
    obj = typeof(obj) === 'string' ? obj.replace(',', '.') : obj;
    return !isNaN(parseFloat(obj)) && isFinite(obj) && Object.prototype.toString.call(obj).toLowerCase() !== '[object array]';
};

module.exports.log = log;
module.exports.getDisplayName = getDisplayName;
module.exports.getDistance = getDistance;
module.exports.getScore = getScore;
module.exports.compare = compare;