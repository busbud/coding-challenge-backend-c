'use strict';

var City       = require('../../models/city');
var Suggestion = require('../../models/suggestion');

var transformCityToSuggestion = function(citiesSuggest) {
    var suggestions = [];

    citiesSuggest.map(function(item) {

        var suggestion = new Suggestion({
            name        : item.name,
            longitude   : item.long,
            latitude    : item.lat,
            score       : 1
        });

        suggestions.push(suggestion);
    });

    return suggestions;
};

var suggestionsController = class SuggestionsController {


    /**
     *
     * /suggestions
     *
     * @queryStringParams q {String}
     * @queryStringParams latitude  {Float} - optionnal
     * @queryStringParams longitude {Float} - optionnal
     * @queryStringParams radius    {Float} - optionnal, radius use it if you want to return results in specific zone.
     *                                        Used only if longitude & latitude are passed.
     *                                        Default value is 5000 meters
     *
     * @params req
     * @params res
     * */
    get(req, res) {

        res.status(200);

        var queryParameter = req.query.q.toString();

        var query = City.find({});
        var where = {
            '$or' : [{
                name : new RegExp(queryParameter, 'i')
            }, {
                alt_name : new RegExp(queryParameter, 'i')
            }]
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
            .exec(function(error, citiesSuggest) {

                // Return a 500 response with the error object
                if(error)
                    return res.status(500).send({type:'error', error: error});

                // If the `name` criteria fetch no records
                // Set the response header to 404
                if(citiesSuggest.length == 0)
                    res.status(404);



                // Send the results
                return res.json({
                    suggestions : transformCityToSuggestion(citiesSuggest)
                });
            });
    }
};

module.exports = new suggestionsController();