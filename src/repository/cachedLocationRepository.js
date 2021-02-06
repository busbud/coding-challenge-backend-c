const NodeCache = require('node-cache');

const LocationRepository = require('./locationRepository');

const queryLocationCache = new NodeCache({ stdTTL: 100, checkperiod: 120 });

// TODO: we could improve this to use redis as a caching layer
class CachedLocationRepository {
    static getLocations(query) {
        const cachedLocations = queryLocationCache.get(query);

        if (cachedLocations) {
            return cachedLocations;
        }

        const locations = LocationRepository.getLocations(query);

        queryLocationCache.set(query, locations);

        return locations;
    }
}

module.exports = CachedLocationRepository;
