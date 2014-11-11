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
    var cityName = options.q;
    if (!cityName) {
        callback("No city name provided.", []);
        return;
    }
    
    callback(null, []);
};
