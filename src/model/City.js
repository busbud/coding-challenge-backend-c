/**
 * @class City
 */

/**
 * @constructor
 * @param options
 * @param {string} options.name
 * @param {string} options.country
 * @param {number} options.population
 * @param {number} options.lat
 * @param {number} options.long
 */
function City(options) {
	this.name = options.name;
	this.country = options.country;
	this.population = options.population;
	this.latitude = options.lat;
	this.longitude = options.long;
}

/**
 * Computes distance with reference latitude and longitude.
 * @param {number} refLatitude in degrees
 * @param {number} refLongitude in degrees
 */
City.prototype.distanceWith = function(refLatitude, refLongitude) {
	var earthRadius = 6371; // km
    var refLatRad = toRadians(refLatitude);
    var latRad = toRadians(this.latitude);
    var deltaLatRad = toRadians(this.latitude-refLatitude);
    var deltaLonRad = toRadians(this.longitude-refLongitude);

    // See formula at http://andrew.hedges.name/experiments/haversine/
    var a = Math.sin(deltaLatRad/2) * Math.sin(deltaLatRad/2) +
            Math.cos(refLatRad) * Math.cos(latRad) *
            Math.sin(deltaLonRad/2) * Math.sin(deltaLonRad/2);
    var c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1-a));

    return earthRadius * c;
};

/**
 * Converts degrees to radians.
 * @private
 */
function toRadians(a) {
    return a * Math.PI / 180;
}

module.exports = City;