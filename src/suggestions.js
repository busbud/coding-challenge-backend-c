var parser = require('./parse.js');


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
        return;
    }

    parser.parseGeoNamesGazetteerTsvFile(newOptions, callback);
};

/**
 * @param {Object} options
 * @param {string} options.q
 * @param {string} options.latitude
 * @param {string} options.longitude
 * @return {Object} New options
 * @return {string} return.q
 * @return {number} return.latitude
 * @return {number} return.longitude
 * @throws
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
            // 
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

    return {
        q : cityName,
        latitude : latitude,
        longitude : longitude
    };
}

/**
 * @param {string} coord
 * @return {number}
 * @private
 * @throws
 */
function parseCoordinate(coord) {
    var floatCoord = parseFloat(coord);
    if (isNaN(floatCoord)) {
        throw new Error("Invalid coordinate " + coord + ".");
    }

    return floatCoord;
}
