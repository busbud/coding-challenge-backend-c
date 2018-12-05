const distance = require('./utils/distance');
const dataFile = require('./utils/dataFile');

var file;
var cities;
var cache = {};
var cacheIdx = 0;
var cacheMaxSize = 10;

module.exports.getElements = 
/**
 * Read cities from file or cache and run search with specific term
 * @param {string} term Term to search in cities file
 * @param {number} limit Limit the number of results 
 */
function (term, limit, lat, long) {

    return new Promise(function(resolve, reject) {

        var key = getCacheKey(term, limit, lat, long);

        if (isSearchInCache(key)) {
            resolve(getSearchFromCache(key));
        }

        file = dataFile.import();

        file.then(function(result) {

            cities = result;

            var searchResult = search(term, limit, lat, long);
            addSearchToCache(key, searchResult);
            resolve(searchResult);
    
        }, function(err) {
            reject(err);
        });

    });
}

function isSearchInCache(key) {
    return key in cache;
}

function getCacheKey(term, limit, lat, long) {
    return term+"#"+limit+"#"+lat+"#"+long;
}

function getSearchFromCache(key) {
    return cache[key];
}

function addSearchToCache(key, value) {
    if (cacheIdx > cacheMaxSize) {
        cache = {}
        cacheIdx = 0;
    }
    cacheIdx++;
    cache[key] = value;
}

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
        var name = city.name;
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

        var admin = city.country === "CA" ? getCode(city.admin1) : city.admin1;

        var score = distance.get(term.toLowerCase(), name.replace(/\./g, '').replace(/,/g, '').toLowerCase(), lat, long, city.lat, city.long);

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

function getCode(code) {
    switch(code) {
        case "01":
            return "AB";
        case "02":
            return "BC";
        case "03":
            return "MB";
        case "04":
            return "NB";
        case "05":
            return "NL";
        case "07":
            return "NS";
        case "08":
            return "ON";
        case "09":
            return "PE";
        case "10":
            return "QC";
        case "11":
            return "SK";
        case "12":
            return "YT";
        case "13":
            return "NT";
        case "14":
            return "NU";
        default:
            return code;
    }
}