const utils = require('./utils');
/**
 * Calculate a score based on name
 * Based on the fact that name is a filtered value of cities based on name;
 * @param {*} name 
 * @param {*} q 
 * @returns between 0 and 1
 */
const scoreName = (c, q) => {
    let ref;
    const lowerName = q.toLowerCase();
    // match on name
    if (c.name.toLowerCase().startsWith(lowerName)) {
        ref = c.name;
    }
    else if (c.ascii.toLowerCase().startsWith(lowerName)) {
        ref = c.ascii;
    }
    else if (c.alternatenames.toLowerCase().startsWith(lowerName)) {
        ref = c.alternatenames.split(',')[0];
    }
    else if (c.alternatenames.toLowerCase().indexOf(`,${lowerName}`) > 0) {
        // take the first, but score can be better with anoter if multiple match
        ref = c.alternatenames.split(',').find(alt => alt.toLowerCase().startsWith(lowerName));
    }
    return q.length / ref.length;
}


/**
 * TODO "algorithm" should be improved with PO.
 * Could be good to add criteria on
 * - population
 * - Number of bus
 * - Number of ticket sold by city
 * - Marketing campaign
 * And differenciate weight of name/lat-long (here equal)
 * @param {*} city 
 * @param {*} q 
 * @param {*} long 
 * @param {*} lat 
 * @returns 
 */
const scoreCoordAndName = (city, q, lat, long) => {
    const scoreForName = scoreName(city, q);
    // score dist = 1- dist in 
    let scoreDist = 1;
    if (long && lat) {
        const dist = utils.distanceLonLat(city.lat, city.long, lat, long);
        if (dist > 10) {
            scoreDist = 1 / dist
        }
    }
    return (scoreForName + scoreDist) / 2;
}


module.exports = { scoreName, scoreCoordAndName }