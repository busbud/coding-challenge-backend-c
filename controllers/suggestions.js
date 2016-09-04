'use strict';

var City       = require('../models/city');
var Suggestion = require('../models/suggestion');
var _          = require('lodash');
var Q          = require('q');

var NodeGeocoder = require('node-geocoder');

var options = {
    provider    : 'google',
    apiKey      : 'AIzaSyDBOJ4sbatzQsvj_FlYoo2eb7DqRfn6J78'
};

var geocoder = NodeGeocoder(options);



var suggestionsController = {

    /**
     *
     * /suggestions
     *
     * @queryStringParams q {String}
     * @queryStringParams latitude  {Float} - Optionnal - Latitude
     * @queryStringParams longitude {Float} - Optionnal - Longitude
     * @queryStringParams radius    {Float} - Optionnal, radius use it if you want to return results in specific zone.
     *                                        Used only if longitude & latitude are passed. In kilometers.
     *                                        Default value is 50 km
     *
     * @params req {Request}
     * @params res {Response}
     * */
    search : function(req, res) {

        res.status(200);

        var queryParameter = req.query.q.toString();

        var queryParameterRegex = new RegExp('^'+queryParameter, 'i');

        var query = City.find({});

        var where = {
            '$or' : [
                {'name' : queryParameterRegex },
                {'ascii' : queryParameterRegex },
                {'alt_name' : queryParameterRegex }
            ]
        };

        if(req.query.longitude && req.query.latitude) {
            where['coords'] = {
                "$near" : {
                    "$geometry": {
                        type: "Point" ,
                        coordinates: [ req.query.longitude , req.query.latitude ]
                    },
                    $maxDistance: req.query.radius || 50000,
                    $minDistance: 0
                }
            }
        }

        query
            .where(where)
            .limit(5)
            .exec(function(error, citiesSuggest) {

                // Return a 500 response with the error object
                if(error)
                    return res.status(500).send({type:'error', error: error});

                // If the `name` criteria fetch no records
                // Set the response header to 404
                if(citiesSuggest.length == 0)
                    res.status(404);

                var promises = [];

                citiesSuggest.map(function(city) {
                    var suggestion = new Suggestion(city);

                    suggestion.setScore({
                        q           : req.query.q,
                        longitude   : req.query.longitude,
                        latitude    : req.query.latitude,
                        radius      : req.query.radius
                    });

                    var promise = geocoder.reverse({ lat : suggestion.latitude, lon : suggestion.longitude })
                        .then(function(res) {

                            suggestion.name  =
                                suggestion.name +
                                ', ' +
                                res[0].administrativeLevels.level1long +
                                ', ' +
                                res[0].country;

                            return suggestion;
                        })
                        .catch(function(err) {
                            console.log(err);
                        });

                    return promises.push(promise);

                });

                return Q.all(promises)
                    .spread(function(suggestions) {

                        // Send the results
                        return res.json({
                            suggestions: _.orderBy(arguments, 'score', 'desc')
                        });
                    });
            });
    }
};

module.exports = suggestionsController;