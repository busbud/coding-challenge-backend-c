'use strict'

const 
	score 	= require('string-score'),
	geodist = require('geodist');

// See 'scoreGeoLocation' function comments for explanations.
const 
	MAX_BONUS 		= 0.3,
	LOW_MALUS 		= -0.5,
	PIVOT_KM 		= 250,	/* in km */
	LONGDIST_KM 	= 5000, /* in km */
	BONUS_SLOPE 	= getSlope(0, MAX_BONUS, PIVOT_KM, 0),
	BONUS_INTERCEPT = getIntercept(BONUS_SLOPE, 0, MAX_BONUS),
	MALUS_SLOPE 	= getSlope(PIVOT_KM, 0, LONGDIST_KM, LOW_MALUS),
	MALUS_INTERCEPT = getIntercept(MALUS_SLOPE, PIVOT_KM, 0);

/**
 * Calculate a city score depending on the requested string and location if provided.
 *
 * @param {object} city The object containing the city information
 * @param {object} query The object containing query string params
 * @returns {undefined} Add a property 'score' to param city
 */ 
function scoreCity(city, query) {
	// Set the base score to string search match
	city.score = scoreStringSearch(city.ascii, query.q);

	// If a location is provided, improve the score
	if (query.latitude != null && query.longitude != null) {

		// Build two objects corresponding to the provided location and the city location
		// using coordinates.
		let querystringLocation = { lat: query.latitude, lon: query.longitude };
		let cityLocation = { lat: city.lat, lon: city.long };

		// Get the score variation based on the two locations
		let geoScoreVar = scoreGeoLocation(querystringLocation, cityLocation);

		// Update the city score by add the location-based score variation to the base score,
		// make sure the final score is within [0, 1] and keep two figures after the decimal point.
		city.score = Math.round((city.score + geoScoreVar).clamp(0, 1) * 100) / 100;
	}
}

/**
 * Calculate a city score regarding string matching.
 *
 * @param {string} target The string to score
 * @param {string} querystring The string to score against
 * @returns {number} The score indicating how well querystring matches target 
 */ 
function scoreStringSearch(target, querystring) {
	// Use string-score module to calculate the score
	return score(target, querystring);
}

/**
 * Calculate a score variation depending on how far two locations are from each other.
 * To do this, use two linear functions:
 *
 *   - The first one gives a positive variation (bonus) from MAX_BONUS (0.3) to 0 rapidely
 *     decreasing as the distance between locations increases from 0 km to PIVOT_KM (500 km).
 * 
 *   .    |
 *      . |
 *    0.3 +.
 *        |   .
 *        |       .
 *     ---+---------+---------
 *        |      500km  .     
 *        |
 *        |
 *
 *   - The second one gives a negative variation (malus) from 0 to -inf, slowly decreasing
 *     as the distance increases from PIVOT_KM to +inf (in real life, this will be limited by
 *     the maximal distance between two land points on earth). To calculate the slope, set a 
 *     point at 5000 km and -0.5 where the variation will go below LOW_MALUS. This point is 
 *     (LONGDIST_KM, LOW_MALUS).
 *
 *       |     .
 *       |        .
 *    ---+----------+----------+-------- ... ---+------
 *       |       500km  .    1000km           5000km
 *       |                 .                    
 *       |                    .                 
 *       |                       .              
 *       |                          .           
 *       |                             .        
 *       |                                .     
 *       |                                   .  
 *  -0.5 +                                      +
 *       |                           (5000,-0.5)    .
 *
 * @param {object} querystringLocation The location provided in the query in geodist format
 * @param {object} cityLocation The city location in geodist format
 * @returns {number} The score variation
 */
function scoreGeoLocation(querystringLocation, cityLocation) {
	// Use geodist module to calculate "as the crow flies" distance between two points
	// It uses the haversine formula (https://en.wikipedia.org/wiki/Haversine_formula)
	var geodistance = geodist(querystringLocation, cityLocation, { unit: 'km' });

	if (geodistance < PIVOT_KM) {
		// From 0 to PIVOT_KM, use the bonus linear function (#1)
		return BONUS_SLOPE * geodistance + BONUS_INTERCEPT;
	} else {
		// From PIVOT_KM, use the malus linear function (#2)
		return MALUS_SLOPE * geodistance + MALUS_INTERCEPT;
	}
	return 0;
}

/**
 * Calculate the slope using two points (x1, y1) and (x2, y2)
 *
 * @param {number} x1 Value on x-axis of first point
 * @param {number} y1 Value on y-axis of first point
 * @param {number} x2 Value on x-axis of second point
 * @param {number} y2 Value on y-axis of second point
 * @returns {number} The function slope
 */ 
function getSlope(x1, y1, x2, y2) {
	return ( ( y2 - y1 ) / ( x2 - x1 ) );
}

/**
 * Calculate the intercept using one point (xxx, yyy) and the slope
 *
 * @param {number} slope Slope of the function
 * @param {number} xxx Value on x-axis of the point
 * @param {number} yyy Value on y-axis of the point
 * @returns {number} The intercept
 */ 
function getIntercept(slope, xxx, yyy) {
	return ( yyy - slope * xxx );
}

/**
 * Use the country code and the admin1 code to retrieve the 2-characters
 * state code according to http://download.geonames.org/export/dump/admin1CodesASCII.txt
 * and https://en.wikipedia.org/wiki/Canadian_postal_abbreviations_for_provinces_and_territories.
 * For USA, the state is actually admin1 value.
 * For Canada, the state is fetched in a static array.
 * For other countries, a different process might be necessary.
 *
 * @param {string} admin1 The first level administration code
 * @param {string} country The ISO-3166 2-letter country code
 * @returns {string} A 2-letter string representing the state
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
 * This app considers only USA and Canada, so we can limit the computation time with a 
 * two-cases scenario. However, a more complex process could be implemented here to
 * include more countries.
 *
 * @param {string} country The ISO-3166 2-letter country code
 * @returns {string} The country name
 */ 
function getCountryName(country) {
	return (country === 'US') ? 'USA' : 'Canada';
}

/**
 * Returns a number whose value is limited to the given range.
 *
 * @param {number} min The lower boundary of the output range
 * @param {number} max The upper boundary of the output range
 * @returns {number} A number in the range [min, max]
 */
Number.prototype.clamp = function(min, max) {
	return Math.min(Math.max(this, min), max);
};


module.exports = {
	getStateCode,
	getCountryName,
	scoreCity
}
