var mongoose = require('mongoose');
var mixins   = require('../mixins/mixins');
var config   = require('../config/config');
var Schema   = mongoose.Schema;

var SuggestionSchema = new Schema({

    // which can be used to disambiguate between similarly named locations
    name : String,

    // longitude
    longitude : Number,

    // latitude
    latitude : Number,

    // Between 0 and 1 (inclusive) indicating confidence in the suggestion (1 is most confident)
    score : Number
});

/**
 * setScore
 *
 * The score depending of two criterion for now :
 *
 * Of the stringMatchingRatio score between the name of the city and the name searched
 *
 * Of the distanceRatio between the centered point wanted by passing a longitude and a latitude and the city.
 * If there is no longitude and latitude provided, this will not be taken in consideration.
 *
 * There is a lot of ways to improve the scoring algorithms :
 * We can take the population as well a criteria
 * We could change the coefficient for each criterion. For now, stringMatching and Distance
 *  have the same coefficient. But the distance would be more important.
 *
 * @params defaultSearchCriteria                    {Object}
 * @params defaultSearchCriteria.q                  {String}    - the initial search criteria
 * @params [defaultSearchCriteria.longitude]        {Float}     - the initial longitude criteria
 * @params [defaultSearchCriteria.latitude]         {Float}     - the initial latitude criteria
 * @params [defaultSearchCriteria.radius]           {Float}     - the initial radius criteria
 * */
SuggestionSchema.methods.setScore = function setScore(defaultSearchCriteria) {

    this.score = 1;
    var distanceRatio = 1.;

    var stringMatchRatio = this.defineStringMatchRatio(defaultSearchCriteria.q.length);

    if(defaultSearchCriteria.longitude && defaultSearchCriteria.latitude) {
        distanceRatio = this.defineDistanceRatio({
                latitude : defaultSearchCriteria.latitude,
                longitude : defaultSearchCriteria.longitude
            }, defaultSearchCriteria.radius
        );
    }

    this.score = calculateScore(stringMatchRatio, distanceRatio);

};

function calculateScore(stringMatchRatio, distanceRatio) {
    var stringMatchCoefficient = 1;
    var distanceCoefficient    = 1;

    return (stringMatchRatio * stringMatchCoefficient) * (distanceRatio * distanceCoefficient);
};

SuggestionSchema.methods.defineStringMatchRatio = function defineStringMatchRatio(stringCriteria) {
    return stringCriteria / this.name.length;
};

/**
 * defineDistanceRatio
 *
 * @param {Object}  coords              the initial longitude criteria
 * @param {float}   coords.longitude    the initial longitude criteria
 * @param {float}   coords.latitude     the initial latitude criteria
 * @param {float}   [radius]            the radius criteria
 *
 * @return {float} the distance ratio
 * */
SuggestionSchema.methods.defineDistanceRatio = function defineDistanceRatio(coords, radius) {
    radius = (radius || config.suggestions.radius);

    var distance = mixins.findDistanceBetweenTwoCoords(
        { lat : this.latitude, lon : this.longitude},
        { lat : coords.latitude, lon : coords.longitude }
    );

    return (distance / radius);
};

var Suggestion = mongoose.model('Suggestion',SuggestionSchema);

module.exports = Suggestion;



