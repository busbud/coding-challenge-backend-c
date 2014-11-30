'use strict';

// @param {Object} query
// @param {string} query.q
// @param {string} query.latitude
// @param {string} query.longitude
// @return {Object} New query
//   @return {string} return.q
//   @return {number} return.latitude
//   @return {number} return.longitude
// @throw
 module.exports = function parseQueryString(query) {
    var cityName = query.q;
   if (!cityName) {
        throw new Error("No city name provided");
    }

    var longitude = null;
    var latitude = null;
    if (query.longitude) {
        try {
            longitude = parseFloat(query.longitude);
            if (isNaN(longitude)) {
                throw new Error("Invalid coordinate " + query.longitude + ".");
            }
        } catch (e) {
            throw "Bad longitude. " + e;
        }
    }
    if (query.latitude) {
        try {
            latitude = parseFloat(query.latitude);
            if (isNaN(longitude)) {
                throw new Error("Invalid coordinate " + query.latitude + ".");
            }
        } catch (e) {
            throw "Bad latitude. " + e;
        }
    }

    return {
        q : cityName.toLowerCase(),
        lat : latitude,
        long : longitude
    };
}