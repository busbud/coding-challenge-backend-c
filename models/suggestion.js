var mongoose = require('mongoose');

var Suggestion = mongoose.model('Suggestion', {

    // which can be used to disambiguate between similarly named locations
    name : String,

    longitude : Number,

    latitude : Number,

    // Between 0 and 1 (inclusive) indicating confidence in the suggestion (1 is most confident)
    score : Number

});

module.exports = Suggestion;