'use strict'

const 
	score = require('string-score'),
	geodist = require('geodist'),
	GeoPoint = require('geopoint');


// Max boni : 0.3 dégressif linéaire jusqu'à 0 de 0 à 100 km
// f(x) = -(0.3/100)*x+0.3
// De 100 à inf. : pénalité dégressive linéaire de 0 à (-0.5, 15000km)
// f(x) = -(0.5/14500)*x+(50/14500)
const MAX_BONUS = 0.3;
const LOW_MALUS = -0.5;
const PIVOT_KM = 500; // in km
const LONGDIST_KM = 5000; // in km, limit where malus will go below LOW_MALUS

// (0, 0.3) = (0, MAX_BONUS)
// (100, 0) = (PIVOT, 0)
//const COEFF_BONUS = getA(0, MAX_BONUS, PIVOT_KM, 0);
const COEFF_BONUS = ( - MAX_BONUS / PIVOT_KM );
//const ORIGORD_BONUS = getB(a, 0, MAX_BONUS);;
const ORIGORD_BONUS = MAX_BONUS;

// (100, 0) = (PIVOT, 0)
// (15000, -0.5) = (MAXDIST, LOW_MALUS)
//const COEFF_MALUS = getA(PIVOT_KM, 0, LONGDIST_KM, LOW_MALUS);
const COEFF_MALUS = ( LOW_MALUS / ( LONGDIST_KM - PIVOT_KM ) );
//const ORIGORD_MALUS = getB(a, PIVOT_KM, 0);
const ORIGORD_MALUS = ( - COEFF_MALUS * PIVOT_KM );

/**
 * Use the country code and the admin1 code to retrieve the 2-characters
 * state code according to http://download.geonames.org/export/dump/admin1CodesASCII.txt
 * and https://en.wikipedia.org/wiki/Canadian_postal_abbreviations_for_provinces_and_territories.
 * For USA, the state is actually admin1 value.
 * For Canada, the state is fetched in a static array.
 * For other countries, a different process might be necessary.
 */ 
function getStateCode(admin1, country) {
	if (country === 'US') {
		return admin1;
	}
	let canadianProvinces = {
		1: "AB",
		2: "BC",
		3: "MB",
		4: "NB",
		5: "NL",
		7: "NS",
		8: "ON",
		9: "PE",
		10: "QC",
		11: "SK",
		12: "YT",
		13: "NT",
		14: "NU",
	};
	return canadianProvinces[admin1];
}

/**
 * Use the country code to get the full country name.
 * This app considers only USA and Canada, we can limit the computation time.
 * However, a more complex process could be implemented here to include more countries.
 */ 
function getCountryName(country) {
	return (country === 'US') ? 'USA' : 'Canada';
}



function scoreCity(city, query) {

	//1. Calculate score
	//1.1 String score
	city.score = scoreStringSearch(city, query.q);
	city.stringScore = city.score;

	//1.2 Distance score bonus-malus
	if (query.latitude != null && query.longitude != null) {

		let querystringLocation = { lat: query.latitude, lon: query.longitude };
		let cityLocation = { lat: city.lat, lon: city.long };

		let geoScoreVar = scoreGeoLocation(querystringLocation, cityLocation, city);

		city.geoScoreVer = geoScoreVar;

		city.score = (city.score + geoScoreVar).clamp(0, 1).toFixed(2);
	}
}

function getA(x1, y1, x2, y2) {
	return ( ( y2 - y1 ) / ( x2 - x1 ) );
}

function getB(a, x, y) {
	return ( y - a * x );
}

function scoreStringSearch(city, querystring) {
	return score(city.name, querystring);
}

function scoreGeoLocation(querystringLocation, cityLocation, city) {
	var geodistance = geodist(querystringLocation, cityLocation, { unit: 'km' });
	city.geodistance = geodistance;
	var geoScoreVar = 0;
	if (geodistance < PIVOT_KM) {
		geoScoreVar = COEFF_BONUS * geodistance + ORIGORD_BONUS;
	} else {
		geoScoreVar = COEFF_MALUS * geodistance + ORIGORD_MALUS;
	}
	return geoScoreVar;
}


/**
 * Returns a number whose value is limited to the given range.
 *
 * @param {Number} min The lower boundary of the output range
 * @param {Number} max The upper boundary of the output range
 * @returns A number in the range [min, max]
 * @type Number
 */
Number.prototype.clamp = function(min, max) {
	return Math.min(Math.max(this, min), max);
};

module.exports = {
	getStateCode,
	getCountryName,
	scoreCity
}
