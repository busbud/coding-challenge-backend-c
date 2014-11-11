var path = require('path');
var parser = require('./parse.js');

// GeoNames Gazetteer constants.
var GEONAMES_SOURCE_FILE_PATH = path.resolve(__dirname, '../data/cities_canada-usa.tsv');
var GEONAMES_FIELDS = {
    population: "population",
    cityName: "name",
    countryCode: "country",
    latitude: "lat",
    longitude: "long"
};
var GEONAMES_FIELDS_ARRAY = getObjectValues(GEONAMES_FIELDS);

// Additional suggestions criteria constants.
var CRITERIA_CITY_POPULATION = 5000;
var CRITERIA_CNTRY_CAN = "CA";
var CRITERIA_CNTRY_USA = "US";


/**
 * Reports the suggestions based on options. An error is reported if no
 * city was specified or no match was found.
 * 
 * @param {Object} options
 * @param {string} options.q
 * @param {string} options.latitude
 * @param {string} options.longitude
 * @param {Function} callback
 * @param {string|null} callback.err
 * @param {Array.<Object>} callback.suggestions
 */
exports.get = function (options, callback) {
    var newOptions;
    try {
        newOptions = parseQueryStringOptions(options);
    } catch (e) {
        callback("Bad get-suggestions options. " + e, []);
    }

    var parserOptions = getGeoNamesGazetteerParserOptions(newOptions);
    parser.parseGeoNamesGazetteerTsvFile(parserOptions, callback);
};

/**
 * @param {Object} options
 * @param {string} options.q
 * @param {string} options.latitude
 * @param {string} options.longitude
 * @return {Object} New options
 * @return {string} return.name
 * @return {string} return.latitude
 * @return {string} return.longitude
 */
function parseQueryStringOptions(options) {
    var cityName = options.q;

    if (!cityName) {
        throw new Error("No city name provided");
    }

    var latitude = options.latitude;
    if (latitude) {
        try {
            parseCoordinate(latitude);
        } catch (e) {
            throw "Bad latitude. " + e;
        }
    } else {
        latitude = null;
    }

    var longitude = options.longitude;
    if (longitude) {
        try {
            parseCoordinate(longitude);
        } catch (e) {
            throw "Bad longitude. " + e;
        }
    } else {
        longitude = null;
    }

    console.log(cityName);
    return {
        name : cityName.replace("é", "e"),//cityName,
        latitude : latitude,
        longitude : longitude
    };
}

/**
 * @param {string} coord
 * @return {number}
 * @throws
 */
function parseCoordinate(coord) {
    var floatCoord = parseFloat(coord);
    if (isNaN(floatCoord)) {
        throw new Error("Invalid coordinate " + coord + ".");
    }

    return floatCoord;
}

/**
 * @param {Object} options
 * @param {string} options.name
 * @param {number} options.latitude
 * @param {number} options.longitude
 * @return {Object}
 * @return {string} return.filePath
 * @return {Function} return.criteriaFn Returns true if valid, false otherwise.
 * @return {Object} return.criteriaFn.fieldValues
 * @return {Function} return.scoreFn Returns the score number between 0 and a few thousand.
 * @return {Object} return.scoreFn.fieldValues
 * @return {Function} return.outputFn Returns the formatted suggestions.
 * @return {Object} return.scoreFn.fieldValuesAndScore
 * @return {number} return.scoreFn.maxScore
 */
function getGeoNamesGazetteerParserOptions(options) {
    var matchRegEx = new RegExp(options.name, "i");
    return {
        filePath: GEONAMES_SOURCE_FILE_PATH,
        fields: GEONAMES_FIELDS_ARRAY,
        criteriaFn: function (fieldValues) {
            // Population criteria.
            if (fieldValues[GEONAMES_FIELDS.population] < CRITERIA_CITY_POPULATION) {
                return false;
            }

            // Country criteria.
            var countryCode = fieldValues[GEONAMES_FIELDS.countryCode];
            if (countryCode !== CRITERIA_CNTRY_CAN && countryCode !== CRITERIA_CNTRY_USA) {
                return false;
            }

            // Match from query string city-name.
            var cityName = fieldValues[GEONAMES_FIELDS.cityName];
            if (!matchRegEx.test(cityName)) {
                return false;
            }

            return true;
        },
        scoreFn: function (fieldValues) {
            var latitude = fieldValues[GEONAMES_FIELDS.latitude];
            var longitude = fieldValues[GEONAMES_FIELDS.longitude];

            if (options.latitude === null || options.longitude === null) {
                return -1;
            }

            return distance(options.latitude, options.longitude, latitude, longitude);            
        },
        outputFn: function (fieldValuesAndScore, maxScore) {
            return {
                name : fieldValuesAndScore[GEONAMES_FIELDS.cityName],
                latitude : fieldValuesAndScore[GEONAMES_FIELDS.latitude],
                longitude : fieldValuesAndScore[GEONAMES_FIELDS.longitude],
                score : normalizeScore(fieldValuesAndScore.score, maxScore)
            };
        }
    };
}

/**
 * Computes the distance between two points using 
 * Haversine formula found at
 * http://www.movable-type.co.uk/scripts/latlong.html
 */
function distance(lat1, lon1, lat2, lon2) {
    var R = 6371; // km
    var φ1 = toRadians(lat1);
    var φ2 = toRadians(lat2);
    var Δφ = toRadians(lat2-lat1);
    var Δλ = toRadians(lon2-lon1);

    var a = Math.sin(Δφ/2) * Math.sin(Δφ/2) +
            Math.cos(φ1) * Math.cos(φ2) *
            Math.sin(Δλ/2) * Math.sin(Δλ/2);
    var c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1-a));

    return R * c;
}

function toRadians(a) {
    return a * Math.PI / 180;
};

function normalizeScore(entryScore, maxScore) {
    if (!maxScore) {
        return 0;
    }

    var newScore = 1 - (entryScore / maxScore);

    return (Math.round(newScore * 10) / 10);
}


// Utility function to extract values.
function getObjectValues(obj) {
    var keys = Object.keys(obj);
    var oValues = [];
    keys.forEach(function (key) {
        oValues.push(obj[key]);
    });
    return oValues;
}