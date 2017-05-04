'use strict';
/**
 * cities.server.routes.controller.js
 * ------------------------------
 */
const geoScoreMax = 0.5;
const geoRadiusBoundaries = [ 0, 10, 22, 46, 100, 215, 464, 1000, 2154, 4642, Infinity]
const geoScoreBinStep = geoScoreMax/geoRadiusBoundaries.length;

var _ = require('lodash');
var Promise = require('bluebird');
var mongoose = require('mongoose');
Promise.promisifyAll( mongoose );
var City = mongoose.model('CitySchema');



module.exports.find = function(req) {
    var q = new RegExp("^" + req.query.q.toLowerCase(), "i");
    return City.findAsync(
        {
            $or: [
                { name: {$regex : q}},
                { ascii: {$regex : q}}
            ]
        },
        {limit: 5},
        {select: 'ascii admin1 country latLng'}
    ).then(function(suggestions) {
            //score names [0;0.5]
            return suggestions
    });
};

module.exports.findNear = function(req) {
    var q = new RegExp("^" + req.query.q.toLowerCase(), "i");
    return City.aggregateAsync([
        {
            $geoNear: {
                query: {$or: [{ name: {$regex : q}},{ ascii: {$regex : q}}]},
                near: {
                  type: "Point",
                  coordinates: [ -73.5673, 45.5017 ]
                },
                spherical: true,
                distanceMultiplier : 0.001,
                distanceField: "distance"
            }
        },
        {
            $bucket: {
                groupBy: "$distance",
                boundaries: geoRadiusBoundaries,
                default: "irrelevant",
                output: {
                    count: { $sum: 1 },
                    cities: { $push:
                        {
                            name: { $concat: [ "$ascii", ", ", "$admin1", ", ", "$country" ] },
                            ascii: "$ascii",
                            latitude: { $arrayElemAt: [ "$latLng.coordinates", 1 ] },
                            longitude: { $arrayElemAt: [ "$latLng.coordinates", 0 ] },
                            distance:"$distance"
                        }
                    }
                }
            }
        }
    ]).then(function(bins) {
            //score names [0;0.5]

            var geoScoreStep = 0;
            var partialBinRange = 0;
            var currBinRange = 0;
            _.forEach(bins, function(bin) {
                geoScoreStep = geoScoreMax - geoRadiusBoundaries.indexOf(bin._id)*geoScoreBinStep;
                partialBinRange = geoScoreBinStep/(bin.count + 1);
                currBinRange = bin.count > 1 ? partialBinRange : 0;
                _.forEach(bin.cities, function(city) {
                    city.score =  (geoScoreStep -  currBinRange).toFixed(2);
                    currBinRange =  currBinRange + partialBinRange;
                });
            });
            return _.flatten(_.map(bins, 'cities'));
    });
};
