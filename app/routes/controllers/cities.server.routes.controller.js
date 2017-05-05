'use strict';
/**
 * cities.server.routes.controller.js
 * ------------------------------
 */
var _ = require('lodash');
var Promise = require('bluebird');
var mongoose = require('mongoose');
Promise.promisifyAll( mongoose );
var constants = require('../../utils/constants');
var scoreUtils = require('../../utils/score-algorithm');
var City = mongoose.model('CitySchema');

module.exports.findStartsWith = function(req) {
    var q = new RegExp("^" + req.query.q.toLowerCase(), "i");
    return City.aggregateAsync([
    {
        $match: {$or: [{ name: {$regex : q}},{ ascii: {$regex : q}}]}
    },
    {
        $project: {
            name: { $concat: [ "$ascii", ", ", "$admin1", ", ", "$country" ] },
            score: { $divide: [ req.query.q.length, { $strLenCP: "$ascii" }] },
            latitude: { $arrayElemAt: [ "$latLng.coordinates", 1 ] },
            longitude: { $arrayElemAt: [ "$latLng.coordinates", 0 ] }
        }
    },
    { $sort: { score: -1}}
    ]).then(function(suggestions){
        for(var i = 0, len=suggestions.length; i<len; ++i) {
            var suggestion = suggestions[i];
            suggestion.score = (Math.round(suggestion.score * 100) / 100);
        }
        return suggestions;
    });
};

module.exports.findNearStartsWith = function(req) {
    var q = new RegExp("^" + req.query.q.toLowerCase(), "i");
    return City.aggregateAsync([
        {
            $geoNear: {
                query: {$or: [{ name: {$regex : q}},{ ascii: {$regex : q}}]},
                near: {
                  type: "Point",
                  coordinates: [ req.query.longitude, req.query.latitude ]
                },
                spherical: true,
                distanceMultiplier : 0.001,
                distanceField: "distance"
            }
        },
        {
            $bucket: {
                groupBy: "$distance",
                boundaries: constants.geoRadiusBoundaries,
                default: "irrelevant",
                output: {
                    count: { $sum: 1 },
                    cities: { $push:
                        {
                            name: { $concat: [ "$ascii", ", ", "$admin1", ", ", "$country" ] },
                            latitude: { $arrayElemAt: [ "$latLng.coordinates", 1 ] },
                            longitude: { $arrayElemAt: [ "$latLng.coordinates", 0 ] },
                            distance:"$distance",
                            nameScore: {$multiply: [{ $divide: [ req.query.q.length, { $strLenCP: "$ascii" }] }, (1 - constants.geoScoreMax)]},
                        }
                    }
                }
            }
        }
    ]).then(function(bins) {
        // Compute geo score
        bins = scoreUtils.geoScore(bins);
        var suggestions = _.flatten(_.map(bins, 'cities'))
        // Format and sum scores
        for(var i = 0, len=suggestions.length; i<len; ++i) {
            var suggestion = suggestions[i];
            suggestion.nameScore = Math.round(suggestion.nameScore * 100) / 100;
            suggestion.geoScore = Math.round(suggestion.geoScore * 100) / 100;
            suggestion.score = Math.round((suggestion.geoScore + suggestion.nameScore) * 100) / 100;
        }
        return _.orderBy(suggestions, 'score', 'desc');
    });
};
