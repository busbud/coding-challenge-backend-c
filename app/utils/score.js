/**
 * @param {Object} city City info
 * @param {number} lat Query lat
 * @param {number} long Query long
 * @return {number} absolute distance from city to query params
 */
const getDistance = (city, lat, long) => {
    let res = 0;

    if (!Number.isNaN(parseFloat(lat))) {
        res += Math.abs(city.lat - lat);
    }
    if (!Number.isNaN(parseFloat(long))) {
        res += Math.abs(city.long - long);
    }

    return res;
};

/**
 * Basic score calculator relying on query length absolute gps distance
 * The closer the query length is to the suggestion name's, the higher the score
 * The lower the difference between city lat/long and provided lat/long, the higher the score
 *
 * @param {Object} city City info
 * @param {number} suggestionsLength Length of suggestions array
 * @param {Object} queryParams
 * @param {number} maxDistance Highest distance between city and lat/long provided
 * @return {number} score
 */
const calculateScore = (city, suggestionsLength, queryParams, maxDistance) => {
    if (suggestionsLength === 1) {
        return 1;
    }
    const {
        q,
        latitude,
        longitude,
    } = queryParams;

    let score = q.length / city.name.length;

    if (latitude || longitude) {
        const distance = getDistance(city, latitude, longitude);
        const distanceScore = ((distance - maxDistance) / -maxDistance);
        score = (score + distanceScore) / 2;
    }

    return score.toFixed(1) * 1;
};

module.exports = {
    getDistance,
    calculateScore,
};
