var City       = require('../models/city');
var Q          = require('q');
var NodeGeocoder = require('node-geocoder');
var _ = require('lodash');

var options = {
    provider    : 'google',
    apiKey      : 'AIzaSyDBOJ4sbatzQsvj_FlYoo2eb7DqRfn6J78'
};

var geocoder = NodeGeocoder(options);

module.exports = {


    /**
     * findSuggestedCities
     *
     * @param {String} name                 the name of the city searched
     * @param {Object} options              the options object
     * @param {float} [options.longitude]   the longitude
     * @param {float} [options.latitude]    the latitude
     * @param {float} [options.radius]      the radius
     *
     * */
    findSuggestedCities : function(name, options) {
        var queryParameterRegex = new RegExp('^'+name, 'i');

        var query = City.find({});

        // We search for names into
        // name , ascii name and alternatives names
        var where = {
            '$or' : [
                {'name'     : queryParameterRegex },
                {'ascii'    : queryParameterRegex },
                {'alt_name' : queryParameterRegex }
            ]
        };

        // If longitude & latitude are passed as option
        // We inject a new where clause to improve the
        // matching results.
        // Also, we can pass a radius parameter to limit the number of results
        if(options.longitude && options.latitude) {
            where['coords'] = {
                "$near" : {
                    "$geometry": {
                        type: "Point" ,
                        coordinates: [ options.longitude , options.latitude ]
                    },
                    $maxDistance: options.radius || 50000,
                    $minDistance: 0
                }
            }
        }

        return query
            .where(where)
            .limit(5)
            .exec()
            .then(function(cities) {

                var promises = [];

                _.forEach(cities, function(city) {

                    // For best results
                    // I'm using the geocoder to increase informations on the results
                    // I'm aware it's not the best place to use it
                    // I would love to put this directly in the parser to retrieve informations juste once
                    // But I'd reached too fast the API key quotas
                    // For this challenge it would be ok (2500 req / days)
                    // That's why I've limited the search results to 5.
                    var promise = geocoder.reverse({ lat : city.latitude, lon : city.longitude })
                        .then(function(res) {
                            city.name  =
                                city.name +
                                ', ' +
                                res[0].administrativeLevels.level1long +
                                ', ' +
                                res[0].country;

                            return city;
                        });

                    return promises.push(promise);
                });

                return Q.all(promises);
            })
    }


}