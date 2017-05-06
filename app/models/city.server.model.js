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
        required: true
    },
    ascii: {
        $type: String,
        trim: true
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

mongoose.model('CitySchema', CitySchema);