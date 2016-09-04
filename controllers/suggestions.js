'use strict';

var Suggestion = require('../models/suggestion');
var _          = require('lodash');
var Q          = require('q');
var CityRepository = require('../repository/cityRepository');

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

        if(! req.query.q)
            return res.status(500).json({ message : 'You have to precise the search criteria', type:"parameterMissing"});

        var queryParameter = req.query.q.toString();

        CityRepository.findSuggestedCities(
            queryParameter,
            {
                longitude:req.query.longitude, latitude : req.query.latitude, radius : req.query.radius
            }
        )
        .then(function(suggestedCities) {

            // If the `name` criteria fetch no records
            // Set the response header to 404
            if(suggestedCities.length == 0)
                res.status(404);

            var suggestions = [];

            // Once all cities are retrieved
            // We create the suggestion object
            // and set it a score depending the initial search criterion
            suggestedCities.map(function(city) {
                var suggestion = new Suggestion(city);

                suggestion.setScore({
                    q           : req.query.q,
                    longitude   : req.query.longitude,
                    latitude    : req.query.latitude,
                    radius      : req.query.radius
                });

                suggestions.push(suggestion);
            });

            // Return the array of suggestion
            // Ordered by score descending
            return res.json({
                suggestions: _.orderBy(suggestions, 'score', 'desc')
            });
        });
    }
};

module.exports = suggestionsController;