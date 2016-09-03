var mongoose = require('mongoose');
var mixins   = require('../mixins/mixins');
var Schema   = mongoose.Schema;

var SuggestionSchema = new Schema({

    // which can be used to disambiguate between similarly named locations
    name : String,

    // longitude
    longitude : Number,

    // latitude
    latitude : Number,

    // Between 0 and 1 (inclusive) indicating confidence in the suggestion (1 is most confident)
    score : Number,

    // TMP
    population : Number
});

/**
 * setScore
 *
 * @params defaultSearchCriteria                    {Object}
 * @params defaultSearchCriteria.q                  {String}    - the initial search criteria
 * @params [defaultSearchCriteria.longitude]        {Float}     - the initial longitude criteria
 * @params [defaultSearchCriteria.latitude]         {Float}     - the initial latitude criteria
 * @params [defaultSearchCriteria.radius]           {Float}     - the initial radius criteria
 * */
SuggestionSchema.methods.setScore = function setScore(defaultSearchCriteria) {

    this.score = 1;

    var stringMatchRatio = this.defineStringMatchRatio(defaultSearchCriteria.q.length);

    var distanceRatio    = this.defineDistanceRatio({
            latitude : defaultSearchCriteria.latitude,
            longitude : defaultSearchCriteria.longitude
        }, defaultSearchCriteria.radius
    );

    this.score = calculateScore(stringMatchRatio, distanceRatio);

};

function calculateScore(stringMatchRatio, distanceRatio) {
    var stringMatchCoefficient = 1;
    var distanceCoefficient    = 1;

    return (stringMatchRatio * stringMatchCoefficient) * (distanceRatio * distanceCoefficient)
};

SuggestionSchema.methods.defineStringMatchRatio = function defineStringMatchRatio(stringCriteria) {
    return stringCriteria / this.name.length;
};

SuggestionSchema.methods.defineDistanceRatio = function defineDistanceRatio(coords, radiusInitial) {
    var radius = 50;
    var distance = mixins.findDistanceBetweenTwoCoords(
        { lat : this.latitude, lon : this.longitude},
        { lat : coords.latitude, lon : coords.longitude }
    );

    if (parseInt(radiusInitial))
        radius = parseInt(radiusInitial);

    return distance / radius;
};

var Suggestion = mongoose.model('Suggestion',SuggestionSchema);


module.exports = Suggestion;



