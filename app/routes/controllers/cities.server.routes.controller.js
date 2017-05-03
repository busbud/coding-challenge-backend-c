'use strict';
/**
 * cities.server.routes.controller.js
 * ------------------------------
 */

var _ = require('lodash');
var mongoose = require('mongoose');
var City = mongoose.model('CitySchema');

module.exports.find = function(req, callback) {
    var q = new RegExp("^" + req.query.q.toLowerCase(), "i");
    City.find()
        .or([{ name: {$regex : q}}, { ascii: {$regex : q}}])
        .limit(25)
        .select('ascii admin1 country latLng')
        .exec(function(err, results) {
            return callback(err, results);
        });
};

module.exports.findNear = function(req, callback) {
    var q = new RegExp("^" + req.query.q.toLowerCase(), "i");
    City.find()
        .or([{ name: {$regex : q}}, { ascii: {$regex : q}}])
        .where('latLng', { $near : {
            //NOTE: coords in GeoJSON are reverted [lng, lat]
            $geometry: { type: "Point",  coordinates: [-72.5477,  46.34515] }
        }})
        .limit(25)
        .select('ascii admin1 country latLng')
        .exec(function(err, results) {
            return callback(err, results);
        });
};