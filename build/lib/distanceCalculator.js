"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.deg2rad = exports.getDistanceFromLatLonInKm = void 0;
// simple algorithm to calculate the score based on the Levenshtein distance between the search term and the city name.
// Function to calculate distance between two points given their latitude and longitude
const getDistanceFromLatLonInKm = (lat1, lon1, lat2, lon2) => {
    const R = 6371; // Radius of the earth in km
    const dLat = deg2rad(lat2 - lat1);
    const dLon = deg2rad(lon2 - lon1);
    const a = Math.sin(dLat / 2) * Math.sin(dLat / 2) +
        Math.cos(deg2rad(lat1)) * Math.cos(deg2rad(lat2)) *
            Math.sin(dLon / 2) * Math.sin(dLon / 2);
    const c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));
    const d = R * c; // Distance in km
    return d;
};
exports.getDistanceFromLatLonInKm = getDistanceFromLatLonInKm;
// Function to convert degrees to radians
function deg2rad(deg) {
    return deg * (Math.PI / 180);
}
exports.deg2rad = deg2rad;
//# sourceMappingURL=distanceCalculator.js.map