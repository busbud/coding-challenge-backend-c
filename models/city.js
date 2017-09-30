let mongoose = require('mongoose');

let cityModel = mongoose.model('City', {
    ident: {
        type: String,
        trim: true
    },
    name: {
        type: String,
        trim: true
    },
    coordinates: [Number]
});

module.exports = cityModel;