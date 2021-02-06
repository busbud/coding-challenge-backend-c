const NodeCache = require('node-cache');

const LocationRepository = require('./locationRepository');

const { DEFAULT_TTL } = require('../constants/cache');

const queryLocationCache = new NodeCache({
    stdTTL: DEFAULT_TTL,
    checkperiod: 120,
    useClones: true, // avoid cache memory exaustion
    maxKeys: 100000, // needs to be adjusted according to pod/machine resources
});

// TODO: we could improve this to use redis as a caching layer and only save the ids
// since the local cache may not work well if there are a lot of pods/machines, specially
// if we try to use partial values as the list for further choises reduction (read more
// at the end of the README.md file)
class CachedLocationRepository {
    static getLocations(query) {
        const cachedLocations = queryLocationCache.get(query);

        if (cachedLocations) {
            const changed = queryLocationCache.ttl(query, DEFAULT_TTL);

            // Just in case the value expires between getting it and updating the TTL
            if (!changed) queryLocationCache.set(query, cachedLocations);

            return cachedLocations;
        }

        const locations = LocationRepository.getLocations(query);

        // TODO: there is an issue here that we are saving the whole
        // location objects which is a lot of wasted memory on repeated entries
        // We could just save the ids and build the response latter at
        // a performance cost. I won't dwell on that since I don't have exact
        // requirements
        queryLocationCache.set(query, locations);

        return locations;
    }
}

module.exports = CachedLocationRepository;
