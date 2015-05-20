var EARTH_RADIUS_KM = 6371;

//using spherical law of cosines
function calculateDistance(lat1, long1, lat2, long2) {
    var radLat1 = lat1 * (Math.PI / 180); //convert to radians
    var radLat2 = lat2 * (Math.PI / 180);
    var deltaLong = (long2 - long1) * (Math.PI / 180);

    return Math.acos(Math.sin(radLat1) * Math.sin(radLat2) + Math.cos(radLat1) * Math.cos(radLat2) * Math.cos(deltaLong) ) * EARTH_RADIUS_KM;
}

module.exports = calculateDistance;