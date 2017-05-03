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
    id: {
        $type: Number,
        required: true
    },
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

CitySchema.plugin(require('../../config/utils/mongoose-visible'), {
    visibleVirtuals: ['label', 'latitude', 'longitude'],
    hidden: ['_id', 'ascii', 'admin1', 'country', 'latLng']
});

CitySchema.virtual('label').get(function() {
    return this.ascii + ', ' + this.country;
});

CitySchema.virtual('latitude').get(function() {
    return this.latLng.coordinates[1];
});

CitySchema.virtual('longitude').get(function() {
    return this.latLng.coordinates[0];
});

mongoose.model('CitySchema', CitySchema);