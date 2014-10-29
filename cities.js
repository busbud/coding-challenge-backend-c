/**
 * High-level module to access cities
 */
var fs = require('fs');
var url = require('url');
var util = require('util');

var es = require('event-stream');
var haversine = require('haversine');
var redis = require('redis');
var S = require('string');


// Heroku RedisToGo setup, if present in environment. Otherwise, use the default settings.
var redisClient;
if (process.env.REDISTOGO_URL) {
    var redisConfig = url.parse(process.env.REDISTOGO_URL);
    redisClient = redis.createClient(redisConfig.port, redisConfig.hostname);
    redisClient.auth(redisConfig.auth.split(':')[1])
} else {
    redisClient = redis.createClient();
}

// Redis key city index
var CITIES_INDEX = 'cities';

// Conversion from Geonames country codes
var GEONAMES_COUNTRIES = {
    'CA': 'Canada',
    'US': 'USA'
};

// Conversion from Geonames canadian provinces
// From http://forum.geonames.org/gforum/posts/list/2902.page
var GEONAMES_CANADIAN_PROVINCES = {
    '00': 'Canada (general)',
    '01': 'Alberta',
    '02': 'British Columbia',
    '03': 'Manitoba',
    '04': 'New Brunswick',
    '05': 'Newfoundland and Labrador',
    '07': 'Nova Scotia',
    '08': 'Ontario',
    '09': 'Prince Edward Island',
    '10': 'Quebec',
    '11': 'Saskatchewan',
    '12': 'Yukon',
    '13': 'Northwest Territories',
    '14': 'Nunavut'
};

// Score criteria weights
var BASE_SCORE_WEIGHTS = {
    query: 7,
    population: 3
};

// Score criteria weights when location available
var LOCATION_SCORE_WEIGHTS = {
    query: 5,
    population: 1,
    distance: 4
};

/**
 * Given a list of cities, computes a relevance score for each of them.
 *
 *
 * @param cities {array} List of cities to score
 * @param query {string} The queried name
 * @param latitude {string} (optional) Optional location to improve search result. Must be paired with a longitude.
 * @param longitude {string} (optional) Optional location to improve search result. Must be paired with a latitude.
 * @returns {array} List of cities, scored.
 */
function scoreCities(cities, query, latitude, longitude) {
    var locationProvided = latitude && longitude;
    var weight = locationProvided ? LOCATION_SCORE_WEIGHTS : BASE_SCORE_WEIGHTS;
    var populationMax = Number.MIN_VALUE;
    var distanceMin = Number.MAX_VALUE;

    if (locationProvided) {
        var source = {
            latitude: latitude,
            longitude: longitude
        };
    }

    // Compute the distance and determine the ranges of values
    cities.forEach(function(city) {
        if (locationProvided) {
            // Set minimum distance, to avoid divide-by-zero errors
            city.distance = haversine(source, {latitude: city.latitude, longitude: city.longitude}) || 0.1;

            distanceMin = Math.min(distanceMin, city.distance);
        }
        populationMax = Math.max(populationMax, city.population);
    });

    // Add the score to the cities
    return cities.map(function(city) {
        var score = 0;

        // Query criteria
        // The longer the city name from query length => greater penalty
        var ratioQueryMatch = query.length / city.asciiName.length;
        score += weight.query * ratioQueryMatch;

        // Population criteria
        // The smaller the population from most populous city => greater penalty
        var ratioHighestPopulation = city.population / populationMax;
        score += weight.population * ratioHighestPopulation;

        if (locationProvided) {
            // Distance criteria
            // The greater the distance from city closest to source => greater penalty
            var ratioShortestDistance = distanceMin / city.distance;
            score += weight.distance * ratioShortestDistance;
        }

        // Round to one decimal, from 10-based weights
        city.score = Math.round(score) / 10;

        return city;
    });
}

/**
 * Performs the initial load of the cities from the specified file.
 * File is expected to be a Geonames formatted TSV
 *
 * @param file {string} Name of the file to load
 * @param callback Will be called once the file has been loaded.
 */
module.exports.loadFile = function(file, callback) {
    // Filter out first line
    var stream = fs.createReadStream(file)
        // Split the lines
        .pipe(es.split('\n'))
        // Split the columns
        .pipe(es.mapSync(function(line) {
            return line.split('\t');
        }))
        // Filter out non-data row (header or empty rows)
        .pipe(es.mapSync(function(line) {
            if (line[0].match(/\d+/)) {
                return line;
            }
        }))
        // Build the city object
        .pipe(es.mapSync(function(columns) {
            return {
                'geonameid': columns[0],
                'name': columns[1],
                'asciiName': columns[2],
                'latitude': columns[4],
                'longitude': columns[5],
                'country': GEONAMES_COUNTRIES[columns[8]] || columns[8],
                'region': GEONAMES_CANADIAN_PROVINCES[columns[10]] || columns[10],
                'population': columns[14]
            };
        }))
        // Add to cities index in Redis
        .pipe(es.mapSync(function(city) {
            var cityName = city.asciiName.toLowerCase();

            return redisClient.hget(CITIES_INDEX, cityName, function(err, result) {
                var citiesForName;
                if (result) {
                    citiesForName = JSON.parse(result);
                    var ids = citiesForName.map(function (c) {
                        return c.geonameid;
                    });
                    var notInList = ids.indexOf(city.geonameid) === -1;

                    if (notInList) {
                        citiesForName.push(city);
                    }
                } else {
                    citiesForName = [city];
                }
                redisClient.hset(CITIES_INDEX, cityName, JSON.stringify(citiesForName));
            });
        }));

    stream.on('error', function(err) {
        callback(err);
    });

    stream.on('end', function() {
        callback();
    });
};

/**
 * Searches the cities for the best matches based on the query and optional location.
 *
 * @param query {string} Partial city name to match.
 * @param latitude {string|number} (optional) Optional location to improve search result. Must be paired with a longitude.
 * @param longitude {string|number} (optional) Optional location to improve search result. Must be paired with a latitude.
 * @param callback Will be passed an array of city objects, ordered by score.
 */
module.exports.search = function(query, latitude, longitude, callback) {
    var citiesList = [];
    if (query) {
        // Lowercase and remove accented characters from query, to match against asciiName
        query = S(query.toLowerCase()).latinise().s;

        // Allow for recursively calling Redis, to exhaust the scan cursor
        var executeScan = function(cursor, callback) {
            redisClient.hscan(CITIES_INDEX, cursor, 'match', query + '*', 'count', 10000, function(err, result) {
                var cursor = result[0];
                var results = result[1];

                // Redis scan returns a list of alternating [key, value, key, value, ...]
                // We are only interested in the values
                for (var i = 1; i < results.length; i = i + 2) {
                    var items = JSON.parse(results[i]);
                    citiesList = citiesList.concat(items);
                }

                // If more results, keep scanning
                if (cursor !== '0') {
                    executeScan(cursor, callback);
                } else {
                    callback();
                }
            });
        };

        executeScan(0, function() {
            // Once scan is done, score the cities, format and sort them
            citiesList =
                scoreCities(citiesList, query, latitude, longitude)
                .map(function(city) {
                    return {
                      name: [city.name, city.region, city.country].join(', '),
                      latitude: city.latitude,
                      longitude: city.longitude,
                      score: city.score
                    };
                })
                .sort(function(a, b) {
                    return b.score - a.score;
                });
            callback(citiesList);
        });
    } else {
        callback(citiesList);
    }
};