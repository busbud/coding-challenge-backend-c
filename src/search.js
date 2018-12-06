const Cache               = require('./utils/cache');
const dataFile            = require('./utils/dataFile');
const distance            = require('./utils/distance');
const geoCode             = require('./utils/geoCode');
const stringNormalization = require('./utils/stringNormalization');

var file;
var cities;
var cache;

module.exports.getElements = 
/**
 * Read cities from file or cache and run search with specific term
 * @param {string} term Term to search in cities file
 * @param {number} limit Limit the number of results 
 * @param {number} lat Latitude searched by user
 * @param {number} long Longitude searched by user
 */
function (term, limit, lat, long) {

    if (cache === undefined) {
        cache = new Cache(10, '#');
    }

    return new Promise(function(resolve, reject) {

        var key = cache.getCacheKey(term, limit, lat, long);

        if (cache.isSearchInCache(key)) {
            resolve(cache.getSearchFromCache(key));
        }

        file = dataFile.import();

        file.then(function(result) {

            cities = result;

            var searchResult = search(term, limit, lat, long);
            cache.addSearchToCache(key, searchResult);
            resolve(searchResult);
    
        }, function(err) {
            reject(err);
        });

    });
};

/**
 * Search term in all the cities
 * @param {string} term Term to search in cities file
 * @param {number} limit Limit the number of results 
 * @param {number} lat Latitude to search
 * @param {number} long Longitude to search
 */
function search(term, limit, lat, long) {


    if (cities === undefined || cities.length === 0) {
        return "Cities undefined";
    }

    var results = [];
    var filteredResults = [];

    cities.forEach(city => {
        var name = city.ascii;
        var country;

        switch(city.country) {
            case "CA":
                country = "Canada";
                break;
            case "US":
                country = "USA";
                break;
            default:
                country = "N/A";
        }

        var admin = city.country === "CA" ? geoCode.getCanadianCode(city.admin1) : city.admin1;

        var score = distance.get(
            stringNormalization.normalize(term), 
            stringNormalization.normalize(name).replace(/\./g, '').replace(/,/g, ''), 
            lat, 
            long, 
            city.lat, 
            city.long);

        // Do not use cities outside CA/US and null score
        if (country !== "N/A" && score >= 0.1) {
            results.push({
                'name': name + ", " + admin + ", " + country,
                'latitude': city.lat,
                'longitude': city.long,
                'score': score
            });
        }       
    });

    if (results.length === 0) {
        return [];
    }

    // Sorting the results by descending score
    results.sort(function(a, b) {
        return b.score - a.score;
    });

    //Limit the result to X (=limit) cities
    for(var i = 0; i < limit; i++) {
        filteredResults.push(results[i]);
    }

    return filteredResults;
}