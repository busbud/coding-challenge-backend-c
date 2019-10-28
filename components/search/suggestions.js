const geo = require('../../services/geo');
const url = require('url');
const querystring = require('querystring');

/*
    search for sugestions based on query input and computes relevance score
    based on the users proximity of the search result to the supplied location

    @param ctxObject - A context object with request information
    @param cb - A callback function
*/
function suggestions(ctxObject, cb) {
    // extract query parameters from the url
    var query = querystring.decode(url.parse(ctxObject.url).query);

    // check that q paramter is present
    if(!query['q'] || query['q'].length===0) {
        cb(400, {}, Error(' query parameter is required '))
    }

    // if both latitude and longitude are passed, ensure that they are numbers
    if(query['latitude'] &&
        query['longitude'] &&
        (isNaN(query['latitude']) || isNaN(query['longitude']))) {
        cb(400, {}, Error('latitude and longitude must be valid numbers'))
    }

    // do a search for city objects that are close to the users query
    geo.find(query['q'], function(results) {

        // if latitude and longitude were passed in the query parameters
        // calculate the distance between the supplied coordinate and the
        // coordinate of the city
        if(query['latitude'] && query['longitude']) {
            let largestDistance = 0;
            results = results
            .filter(city => city['latitude'] && city['longitude'])
            .map((city, index) => {
                city['distance'] = geo.calculateDistance(query['latitude'],
                    query['longitude'],
                    city.latitude,
                    city.longitude
                );
                return city;
            });
        }

        // calculate score for each result and sort in descending order
        results = results
        .map((city) => {
            city['score'] = calculateScore(city);
            delete city['distance'];
            return city;
        })
        .sort((cityA, cityB) => {
            return cityB['score'] - cityA['score'];
        });

        // call server callback
        cb(results.length===0? 404: 200, {
            suggestions: results
        });
    });
}

/*
    Calculate relevance score based on city distance from supplied coordinates

    @param city - city object to calculate distance to
*/
function calculateScore(city) {
    // set default score of 1 for all results that have matched so far
    let score = 1;
    if(city['distance']) {
        // distance is normalized between 0 and 1 by (x - min/max-min)
        // the minimum value being 0, and the maximum value being the largest
        // possible distance between two points on a sphere (diameter * pi * 0.5)
        score = (score + (1 - ( city['distance'] / (12756*(3.142)*0.5)))) / 2;
    }
    return score;
}

module.exports = suggestions;
