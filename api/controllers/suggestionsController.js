const cityManager = require('../modules/cityManagerModule');
const minimumScore = 0.7;

/**
 * getSuggestions delegate the functional search, but it is his responsability to order results the way we want to get them back to the user
 * @param req
 * @param res
 */
exports.getSuggestions = function(req, res) {
    let suggestions = cityManager.findCities(req.query.q, minimumScore);

    if(suggestions.length == 0) {
        res.statusCode = 404;
    }
    else {
        for(let i = 0; i < suggestions.length;i++) {
            //console.log(suggestions[i].name);
        }
    }

    res.json({suggestions : suggestions});
};