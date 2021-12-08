const getCountry = (code) => {
    switch(code) {
        case 'CA': return 'Canada';
        case 'US': return 'USA';
        default: return 'N/A';
    }
}

/**
 * Distance compute
 * source: https://www.movable-type.co.uk/scripts/latlong.html
 * @param {*} city 
 * @param {*} lat 
 * @param {*} long 
 * @return dist in km
 */
 const distanceLonLat = (lat1, lon1, lat2, lon2) => {
    const R = 6371e3; // metres
    const φ1 = lat1 * Math.PI / 180; // φ, λ in radians
    const φ2 = lat2 * Math.PI / 180;
    const Δφ = (lat2 - lat1) * Math.PI / 180;
    const Δλ = (lon2 - lon1) * Math.PI / 180;
    const a = Math.sin(Δφ / 2) * Math.sin(Δφ / 2) +
        Math.cos(φ1) * Math.cos(φ2) *
        Math.sin(Δλ / 2) * Math.sin(Δλ / 2);
    const c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));

    return R * c / 100; // in km
}

module.exports = {getCountry, distanceLonLat}