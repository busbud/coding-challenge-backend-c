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
        type: String,
        trim: true,
        required: true,
        index: 'text'
    },
    ascii: {
        type: String,
        trim: true,
        required: true,
        index: 'text'
    },
    latLng: {
        type: String,
        coordinates: [Number]
    },
    admin1: {
        type: String,
        trim: true,
        required: true
    },
    country: {
        type: String,
        trim: true,
        required: true
    }
}, {
    collection: 'cities'
});

CitySchema.index({latLng: '2dsphere'});

mongoose.model('CitySchema', CitySchema);