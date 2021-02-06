const GeoPoint = require('geopoint');
const fuzz = require('fuzzball');

const COUNTRY_MAP = {
    US: 'USA',
    CA: 'Canada',
};

// Json is easier to work with
const locations = require('../../data/cities_canada-usa.json')
    .map((location) => ({
        // FIXME: I'm not sure if the admin1 param is correct here
        name: `${location.name}, ${location.admin1}, ${COUNTRY_MAP[location.country]}`,
        altName: `${location.name},${location.ascii},${location.alt_name}`,
        latitude: location.lat,
        longitude: location.long,
        geoPoint: new GeoPoint(Number(location.lat), Number(location.long)),
    }));

const DEFAULT_FUZZ_OPTIONS = {
    scorer: fuzz.partial_ratio,
    processor: (choice) => choice.altName,
    // TODO: how much should the limit be?
    limit: 20, // Max number of top results to return, default: 0 (no limit).
    // TODO: how much should the cutoff be?
    cutoff: 70, // Lowest score to return, default: 0
};

// TODO: we could improve this to use a database if we think that we'll have to manage the location list
// The database will probably have fuzzy queries (I know that postgresql has it)
class LocationRepository {
    static getLocations(query) {
        return fuzz.extract(query, locations, DEFAULT_FUZZ_OPTIONS).map((result) => ({
            ...result[0],
            score: result[1] / 100,
        }));
    }
}

module.exports = LocationRepository;
