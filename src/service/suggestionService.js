const GeoPoint = require('geopoint');

const LocationRepository = require('../repository/cachedLocationRepository');

// TODO: I have no idea how much the location should actually increase the score
// I'm using the increment as a sum to the final score, but it could be a direct
// multiplier to the base score, I don't know
const MAX_LOCATION_INCREMENT = 0.2;

const sortByHighestScore = (a, b) => b.score - a.score;

class SuggestionService {
    static getSuggestions(query, latitude, longitude) {
        const locations = LocationRepository.getLocations(query);

        if (locations.length === 0) {
            return locations;
        }

        if (latitude && longitude) {
            const requestPoint = new GeoPoint(Number(latitude), Number(longitude));

            locations.forEach((location) => {
                const distance = requestPoint.distanceTo(location.geoPoint, true);
                const locationMultiplier = distance < 1000 ? 1 : (1000 / distance);

                // eslint-disable-next-line no-param-reassign
                location.score += MAX_LOCATION_INCREMENT * locationMultiplier;

                // eslint-disable-next-line no-param-reassign
                if (location.score > 1) location.score = 1;
            });
        }

        return locations.map((location) => ({
            name: location.name,
            latitude: location.latitude,
            longitude: location.longitude,
            // TODO: how many digits are needed?
            score: Number(location.score.toFixed(2)),
        })).sort(sortByHighestScore);
    }
}

module.exports = SuggestionService;
