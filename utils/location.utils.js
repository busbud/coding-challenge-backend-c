/**
 * location.utils.js
 * Utilities used throughout the application focusing on Location related functions
 */



/**
 * Returns distance of two points in km
 * @param {Decimal} lat1 
 * @param {Decimal} lon1 
 * @param {Decimal} lat2 
 * @param {Decimal} lon2 
 * @returns {Decimal} distance in km
 */
function getDistanceFromLatLonInKm(lat1, lon1, lat2, lon2) {
    /**
     * converts degrees to radians
     * @param {Decimal} deg 
     * @returns {Decimal} radians   
     */
    const deg2rad = (deg) => {
        return deg * (Math.PI / 180)
    }

    const R = 6371; // Radius of the earth in km
    const dLat = deg2rad(lat2 - lat1);  // deg2rad below
    const dLon = deg2rad(lon2 - lon1);
    const a =
        Math.sin(dLat / 2) * Math.sin(dLat / 2) +
        Math.cos(deg2rad(lat1)) * Math.cos(deg2rad(lat2)) *
        Math.sin(dLon / 2) * Math.sin(dLon / 2)
        ;
    const c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));
    const d = R * c; // Distance in km
    return d;
}

module.exports = getDistanceFromLatLonInKm;
