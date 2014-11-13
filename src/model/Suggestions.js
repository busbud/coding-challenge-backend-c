var SuggestedCity = require("./SuggestedCity.js");

/**
 * @class Suggestions
 * 
 * This class manages a collection of SuggestedCity objects 
 * that fit the Suggested city criteria.
 */

/**
 * @constructor
 */
function Suggestions() {
	this.suggestedCities = [];
}

/**
 * This method adds a city based on field values and query string 
 * options, if it fits the criteria.
 *
 * @param {Object} fieldValueOptions Field values.
 * @param {string} fieldValueOptions.name
 * @param {string} fieldValueOptions.country
 * @param {number} fieldValueOptions.population
 * @param {number} fieldValueOptions.lat
 * @param {number} fieldValueOptions.long
 *
 * @param {Object} queryOptions Query string options.
 * @param {string} queryOptions.q
 * @param {number} [queryOptions.latitude]
 * @param {number} [queryOptions.longitude]
 */
Suggestions.prototype.add = function(fieldValueOptions, queryOptions) {
	var suggestedCity = new SuggestedCity(fieldValueOptions);
	if (suggestedCity.fitsCriteria(queryOptions.q)) {
		// Keep only interesting rows.
        suggestedCity.setScore(queryOptions.latitude, queryOptions.longitude);
        this.suggestedCities.push(suggestedCity);
    }
}

/**
 * @return {boolean} True if no city has been added with fitting criteria, otherwise false.
 */
Suggestions.prototype.isEmpty = function() {
	return (this.suggestedCities.length === 0);
}

/**
 * @return {Array.<Object>} Array of sorted suggestion objects.
 */
Suggestions.prototype.get = function() {
	var finalOutput = [];

    var orderedCities = this.suggestedCities.sort(function (cityA, cityB) {
        return (cityA.score - cityB.score);
    });

    var maxScore;
    var sampleScore = orderedCities[0].score;
    if (sampleScore === null) {
        maxScore = null;
    } else {
        maxScore = orderedCities[orderedCities.length - 1].score;
    }

    orderedCities.forEach(function (city) {
        finalOutput.push(city.output(maxScore, orderedCities.length));
    });

    return finalOutput;
}

module.exports = Suggestions;