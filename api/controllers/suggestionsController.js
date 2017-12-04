const cityManager = require('../modules/cityManagerModule');
const minimumScore = 0.3;
const maxResults = 10;

/**
 * getSuggestions delegate the functional search, but it is his responsability to order results the way we want to get them back to the user
 * Sorting is made as follow : first level score desc, second level if scores are the same, distance asc
 * params of the query :
 * - q : searched String (mandatory)
 * - longitude : longitude used to calculate distance (mandatory if latitude is present)
 * - latitude : latitude used to calculate distance (mandatory if longitude is present)
 * - nbr : maximum number of results expected
 * - ms : minimum score to reach in order to retrieve results
 * @param req
 * @param res
 */
exports.getSuggestions = function(req, res) {
    let currentMinScore = req.query.ms || minimumScore;
    let currentMaxResults = req.query.nbr || maxResults;
    let suggestions = cityManager.findCities(req.query.q, req.query.longitude, req.query.latitude, currentMinScore);
    if(suggestions.length == 0) {
        res.statusCode = 404;
    }
    else {
        suggestions.sort((a,b) => {
            if(a.score == b.score) {
                return a.distance - b.distance;
            } else {
                return b.score - a.score;
            }
        });

        if (suggestions.length > currentMaxResults) suggestions = suggestions.slice(0, currentMaxResults);

    }

    res.json({suggestions : suggestions});
};