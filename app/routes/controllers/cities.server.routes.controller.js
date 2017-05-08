'use strict';

const nameCompletionScoreKey = 'nameCompletionScore';
const nameScoreKey = 'nameScore';
const geoScoreKey = 'geoScore';

/**
* Module dependencies.
*/

var _ = require('lodash'),
    mongoose = require('mongoose'),
    constants = require('../../utils/constants'),
    scoreUtils = require('../../utils/score-algorithm'),
    City = mongoose.model('CitySchema');

/**
* Find and score city matches based on a search string
*
* @param {Object} req - request
* @param {Object} req.query - request query param(s)
* @param {string} req.query.q - city name to match
* @return {Promise}
* @resolve {Object[]} suggested cities
*/

module.exports.findStartsWith = function(req) {
    var q = new RegExp("^" + req.query.q.toLowerCase(), "i");
    return City.aggregateAsync([
    {
        $match: {$or: [{ name: {$regex : q}},{ ascii: {$regex : q}}]}
    },
    {
        $project: {
            _id : 0,
            name: { $concat: [ "$ascii", ", ", "$admin1", ", ", "$country" ] },
            [nameCompletionScoreKey]: {$divide: [ req.query.q.length, { $strLenCP: "$ascii" }] },
            latitude: { $arrayElemAt: [ "$latLng.coordinates", 1 ] },
            longitude: { $arrayElemAt: [ "$latLng.coordinates", 0 ] }
        }
    }
    ]).then(suggestions => {
        return _.orderBy(scoreUtils.nameScore(suggestions, nameCompletionScoreKey, 'score'), 'score', 'desc');
    });
};

/**
* Find and score city matches based on a search string
* and geographic coordinates
*
* @param {Object} req - request
* @param {Object} req.query - request query param(s)
* @param {string} req.query.q - city name to match
* @param {string} req.query.latitude - city name to match
* @param {string} req.query.longitude - city name to match
* @return {Promise}
* @resolve {Object[]} suggested cities
*/

module.exports.findNearStartsWith = function(req) {
    var q = new RegExp("^" + req.query.q.toLowerCase(), "i");
    req.query.latitude = parseFloat(req.query.latitude);
    req.query.longitude = parseFloat(req.query.longitude);

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
                    suggestions: { $push:
                        {
                            name: { $concat: [ "$ascii", ", ", "$admin1", ", ", "$country" ] },
                            latitude: { $arrayElemAt: [ "$latLng.coordinates", 1 ] },
                            longitude: { $arrayElemAt: [ "$latLng.coordinates", 0 ] },
                            distance:"$distance",
                            [nameCompletionScoreKey]: { $divide: [ req.query.q.length, { $strLenCP: "$ascii" }]},
                        }
                    }
                }
            }
        }
    ]).then(bins => {
        // Compute geo scores within bins and flatten results
        var suggestions = _.flatten(_.map(scoreUtils.geoScore(bins, geoScoreKey), 'suggestions'));

        // Compute name scoring
        scoreUtils.nameScore(suggestions, nameCompletionScoreKey, nameScoreKey);

        // Sum both scores by relative weight
        for(var i = 0, len=suggestions.length; i<len; ++i) {
            var suggestion = suggestions[i];
            suggestion.score = Math.round((suggestion[geoScoreKey]*constants.findNearStartsWithGeoWeight
                + suggestion[nameScoreKey]*constants.findNearStartsWithNameWeight) * 100) / 100;
        }

        // Only case of absolute confidence
        if (suggestions.length === 1) {
            suggestions[0].score = 1;
        }

        return _.orderBy(suggestions, 'score', 'desc');
    });
};
