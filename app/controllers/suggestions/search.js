const {
    fetchCountries,
    fetchStates,
    fetchCities,
    getDistance,
    calculateScore,
} = require('../../utils');

/**
 * @param {Object} city City info
 * @param {String} queryName `q` param lower cased without dashes
 * @return {boolean} match
 */
const isMatching = (city, queryName) => {
    const name = city.name.toLowerCase().replace('-', ' ');
    const ascii = city.ascii.toLowerCase().replace('-', ' ');
    return name.startsWith(queryName) ||
        ascii.startsWith(queryName);
};

const search = (req, res, next) => {
    try {
        const {
            q,
            latitude,
            longitude,
        } = req.query;

        if (!q) {
            return res
                .status(400)
                .send({
                    code: 'BadRequestError',
                    message: 'Missing `q` parameter',
                });
        }

        const countries = fetchCountries(res);
        const states = fetchStates(res);
        const cities = fetchCities(res);
        const maxConfidenceIndexes = [];
        const queryName = q.toLowerCase()
            .replace(/-/g, ' ')
            .replace(/\s\s+/g, ' ')
            .trim();
        let maxDistance = 0;

        const suggestions = cities
            .filter((city) => {
                const match = isMatching(city, queryName);
                if (match && (latitude || longitude)) {
                    const distance = getDistance(city, latitude, longitude);

                    maxDistance = distance > maxDistance
                        ? distance
                        : maxDistance;
                }
                return match;
            })
            .map((city, idx, self) => {
                const {
                    name,
                    country,
                    admin1,
                    lat,
                    long,
                } = city;
                const score = calculateScore(
                    city,
                    self.length,
                    req.query,
                    maxDistance,
                );

                if (score === 1) {
                    maxConfidenceIndexes.push(idx);
                }
                return {
                    name: `${name}, ${
                        states[`${country}.${admin1}`].name || admin1
                    }, ${countries[country].Country}`,
                    latitude: lat,
                    longitude: long,
                    score,
                };
            });

        // Ensure there are no multiple suggestions with maximum confidence
        // Happens when multiple cities have same name and only `q` is provided
        // e.g /suggestions?q=london
        if (maxConfidenceIndexes.length > 1) {
            maxConfidenceIndexes.forEach((idx) => {
                suggestions[idx].score = 0.9;
            });
        }
        return res
            .status(suggestions.length ? 200 : 404)
            .send({
                suggestions: suggestions
                    .sort((a, b) => b.score - a.score)
                    .slice(0, 10),
            });
    } catch (error) {
        console.error('Error suggestions:search', error);
        return res.status(500).send({
            code: 'InternalServerError',
            message: error,
        });
    }
};

module.exports = search;
