const url = require('url');
const cityModel = require('./../models/city');
const validator = require('./../services/validator');
const levenshtein = require('./../services/levenshtein-distance');

exports.handleRequest = (req, res) => {

    let queryParams = getQueryStringParams(req);

    if(queryParams.q) {
        let search = queryParams.q;
        cityModel.find({ name: new RegExp(`^${search}`, 'i') }).then(cities => {
            let suggestions = formatCities(cities, queryParams);
            return sendResponse(res, suggestions);
        });
    }
    else {
        sendResponse(res, []);        
    }
}

function sendResponse(res, results) {
    res.setHeader('Content-Type', 'application/json');
    res.statusCode = results.length ? 200 : 404;
    return res.end(JSON.stringify({
        suggestions: results
    }));
}

// Returns query params
function getQueryStringParams(req) {
    return url.parse(req.url, true).query;
}

function hasLocation(queryParams) {
    return validator.coordinateIsValid(queryParams.latitude) && validator.coordinateIsValid(queryParams.longitude);
}

function formatCities(cities, queryParams) {

    let suggestions = [];
    let hasLocationParams = hasLocation(queryParams);

    if(cities.length > 0) {    
        
        suggestions = cities.map((city, index) => {

            var suggestion = {
                name : `${city.name}, ${city.admin1}, ${city.country}`,
                latitude: city.location[0],
                longitude: city.location[1]
            }
            
            if(!hasLocationParams) {
                suggestion.score = levenshtein.compare(queryParams.q, city.name);
            }

            return suggestion;

        });
    }

    return sortSuggestions(suggestions);
}

function sortSuggestions(suggestions) {
    return suggestions.sort((a,b) => b.score - a.score);
}
