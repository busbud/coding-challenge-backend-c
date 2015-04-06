var _ = require('underscore');
var score = require('../utils/score');
var trie_format_string = require('../utils/string').trie_format_string
var admin_codes = require('../utils/admin_codes');
var coordinates = require('../utils/coordinates');
var logger = require('../logger').logger;

function get_admin_code(admin_code) {
    return admin_codes.CA[admin_code] || admin_code;
}

module.exports = function(app, city_trie, city_info) {
    app.get('/suggestions', function(req, res, next) {
        if (!req.query.q) {
            res.status(400);
            return res.json({
               error: 'Must provide a City to your query (q)' 
            });
        }
        
        var coordinatesFormattedCorrectly = coordinates.validCoordinate({
            latitude: req.query.latitude,
            longitude: req.query.longitude
        });
        
        if (!coordinatesFormattedCorrectly) {
            res.status(400);
            return res.json({
                error: 'Latitude and Longitude must be valid numbers or both not provided'
            });
        }
        
        logger.info('searching Trie for words with prefix \'' + req.query.q + '\'')
        var suggestion_city_names = city_trie.wordsWithPrefix(trie_format_string(req.query.q));
        
        var suggestions = _.flatten(_.map(suggestion_city_names, function(city_name) {
            return _.map(city_info[city_name], function(info) {
                // clone info object so that we don't actually return a reference to
                // the object inside city_info
                return _.clone(info);
            });
        }));
        
        if (!suggestions.length) {
            res.status(404);
            return res.json({
                suggestions: []
            });
        }
        
        logger.info('calculating suggestion scores');
        suggestions_with_scores = _.map(suggestions, function(suggestion) {
            var city_score = score(req.query.q, 
                                   req.query.latitude, 
                                   req.query.longitude, 
                                   suggestion
                                  );
            
            suggestion = _.extend(suggestion, {
                score: city_score,
                name: suggestion.name + ', ' + 
                        get_admin_code(suggestion.admin1) + ', ' + 
                        suggestion.country,
                latitude: suggestion.lat,
                longitude: suggestion.long
            });
            
            return _.omit(suggestion, 'admin1', 'country', 'lat', 'long');
        });
        
        return res.json({
            suggestions: _.sortBy(suggestions_with_scores, function(suggestion) {
                return 1/suggestion.score;
            })
        });
    });
}