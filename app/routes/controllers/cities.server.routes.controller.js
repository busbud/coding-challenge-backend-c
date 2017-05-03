'use strict';
/**
 * cities.server.routes.controller.js
 * ------------------------------
 */

var Promise = require('bluebird');
var mongoose = require('mongoose');
Promise.promisifyAll( mongoose );
var City = mongoose.model('CitySchema');

module.exports.find = function(req) {
    var q = new RegExp("^" + req.query.q.toLowerCase(), "i");
    return City.findAsync({
        $or: [
            { name: {$regex : q}},
            { ascii: {$regex : q}}
        ]
    }, {limit: 5}, {select: 'ascii admin1 country latLng'});
};

module.exports.findNear = function(req) {
    var q = new RegExp("^" + req.query.q.toLowerCase(), "i");
    return City.findAsync({
      $and: [{
        $or: [
            { name: {$regex : q}},
            { ascii: {$regex : q}}
        ]},{
        'latLng': { $near : {
            $geometry: { type: "Point",  coordinates: [req.query.longitude, req.query.latitude] }
        }} }
      ]
    }, { limit: 5 }, { select: 'ascii admin1 country latLng' });
};