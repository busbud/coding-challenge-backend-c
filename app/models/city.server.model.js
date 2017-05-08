'use strict';
/**
 * Module dependencies.
 */

var mongoose = require('mongoose'),
    Schema = mongoose.Schema;

/**
 * City Schema
 */

var CitySchema = new Schema({
    name: {
        $type: String,
        trim: true,
        required: true,
        index: 'text'
    },
    ascii: {
        $type: String,
        trim: true,
        required: true,
        index: 'text'
    },
    latLng: {
        type: String,
        coordinates: [Number]
    },
    admin1: {
        $type: String,
        trim: true,
        required: true
    },
    country: {
        $type: String,
        trim: true,
        required: true
    }
}, {
    typeKey: '$type',
    collection: 'cities'
});

CitySchema.index({latLng: '2dsphere'});

CitySchema.pre('save', function(next) {
    if (this.country === "CA"){
        this.admin1 = CA_StateCodes[this.admin1];
    }
    next();
});

mongoose.model('CitySchema', CitySchema);

var CA_StateCodes = {
    1: 'AB',
    2: 'BC',
    3: 'MB',
    4: 'NB',
    5: 'NL',
    7: 'NS',
    8: 'ON',
    9: 'PE',
    10: 'QC',
    11: 'SK',
    12: 'YT',
    13: 'NT',
    14: 'NU'
};