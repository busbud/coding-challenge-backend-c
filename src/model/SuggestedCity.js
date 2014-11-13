var City = require('./City.js');

/**
 * @class SuggestedCity
 */

/**
 * @constructor
 * @param options
 * @param {string} options.name
 * @param {string} options.country
 * @param {number} options.population
 * @param {number} options.lat
 * @param {number} options.long
 */
function SuggestedCity(options) {
	this.city = new City(options);
	this.normalizedCityName = normalizeCityNameString(options.name);
	this.score = null;
}

/**
 * @cfg
 * @private
 */
var CRITERIA_CITY_POPULATION = 5000;

/**
 * @cfg
 * @private
 */
var CRITERIA_CNTRY_CAN = "CA";

/**
 * @cfg
 * @private
 */
var CRITERIA_CNTRY_USA = "US";


/**
 * Used to filter parsed strings.
 * @param {string} cityNameSearchString
 * @return {boolean}
 */
SuggestedCity.prototype.fitsCriteria = function(cityNameSearchString) {
	// Population criteria.
    if (this.city.population < CRITERIA_CITY_POPULATION) {
        return false;
    }

    // Country criteria.
    var countryCode = this.city.country;
    if (countryCode !== CRITERIA_CNTRY_CAN && countryCode !== CRITERIA_CNTRY_USA) {
        return false;
    }

    return isPotentialCityNameMatch(this.normalizedCityName, cityNameSearchString);
};

/**
 * @param {string} normalizedCityName
 * @param {string} searchString
 * @return {boolean}
 * @private
 */
function isPotentialCityNameMatch(normalizedCityName, searchString) {
	var containsCharactersRegExp =  createRegExpFromQueryStringInput(searchString);

	// TODO: Improve with more clever search: approximate string search, or more 
	// common patterns for users: "Saint(e)" for "St" or "Ste".

	return containsCharactersRegExp.test(normalizedCityName);
}

/**
 * @param {string}
 * @return {RegExp}
 */
function createRegExpFromQueryStringInput(searchString) {
	var newSearchString = normalizeCityNameString(searchString);

	// Special RegExp characters...
	newSearchString = newSearchString.replace(/[-[\]{}()*+?.,\\^$|#\s]/g, "\\$&");

	return new RegExp(newSearchString);
}

/**
 * E.g. "Montréal" becomes "montreal"
 * @param {string} cityName
 * @return {string}
 * @private
 */
function normalizeCityNameString(cityName) {
	var newCityName = cityName.toLowerCase();

	var frenchNormalization = {
		"é":"e",
		"è":"e",
		"ê":"e",
		"à":"a",
		"â":"a",
		"î":"i",
		"ô":"o",
		"û":"u",
		"ù":"u"
	};

	// TODO Research non-ASCII characters in the list of cities.

	for (var charac in frenchNormalization) {
		newCityName = newCityName.replace(charac, frenchNormalization[charac]);
	}

	return newCityName;
}

/**
 * @param {number|null} queryLatitude
 * @param {number|null} queryLongitude
 */
SuggestedCity.prototype.setScore = function(queryLatitude, queryLongitude) {
	if (queryLatitude !== null && queryLongitude !== null) {
		this.score = this.city.distanceWith(queryLatitude, queryLongitude);

		// TODO: Improve with compound score with string approximation 
		// given results are not filtered out.
	}

	return this.score;
};

/**
 * @param {number|null} highestScore
 * @return {Object}
 */
SuggestedCity.prototype.output = function(highestScore, nScores) {
	var finalScore = computeFinalScore(this.score, highestScore, nScores);	

	return {
		name: this.city.name,
		latitude: this.city.latitude,
		longitude: this.city.longitude,
		score: finalScore
	};
};

/**
 * @param {number} score
 * @param {number} highestScore
 * @return {number}
 */
function computeFinalScore(score, highestScore, nScores) {
	if (highestScore !== null && score !== null) {
		if (highestScore === 0) {
			// Special case: the only result has equal query lat. and long.
			return 1;
		} else if (nScores === 1 && score === highestScore) {
			// Special case: the only result has equal query lat. and long.
			return 1;

			// TODO Verify no two cities (or entries with different names) can be 
			// recorded at the same latitude and longitude. Else nScores could be 
			// greater than 1.
		}
    } else {
    	// No useful scores.
    	return 0;
    }

	var finalScore = 1 - (score / highestScore); // lowest score is better (distance smaller).
	return (Math.round(finalScore * 10) / 10); // round to one floating point.
}

module.exports = SuggestedCity;