const mongoose = require('mongoose');

let citySchema = new mongoose.Schema({
    name: {
        type: String,
        trim: true
    },
    location: { 
        type: [], 
        index: '2d'
    },
    country: {
        type : String,
        trim: true
    },
    state : {
        type: String,
        trim: true
    }
});

const cityModel = mongoose.model('City', citySchema);

module.exports = cityModel;