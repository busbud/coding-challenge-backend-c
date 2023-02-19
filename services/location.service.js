/**
 * Abstraction over the location data implementation. In this case its a Trie but it could something else
 */
const cache = require('../lib/cache');
const getDistanceFromLatLonInKm = require('../utils/location.utils');

/**
 * Assigns distance to each location if coordinates provided
 * @param {Array<Location>} locations 
 * @param {Object} coordinates  {latitude, longitude} provided by user in request
 * @returns 
 */
function assignDistancesToLocationsArray(locations, coordinates) {
    return locations.map((loc) => {
        const distance = getDistanceFromLatLonInKm(loc.latitude, loc.longitude, coordinates.latitude, coordinates.longitude);
        return { ...loc, distance };
    });
}

/**
 * Sorts by distance
 * @param {Location} a 
 * @param {Location} b 
 * @returns 
 */
function sortByDistance(a, b) {
    return a.distance > b.distance ? 1 : -1;
}

/**
 * Assigns a score to each location based on the distance if provided elses assigns a base score
 * @param {Array<Location>} locations Array of location objects
 * @returns 
 */
function scoreLocations(locations) {
    // Ranges for scoring
    const ranges = [100000, 50000, 20000, 10000, 5000, 2000, 1000, 500, 250, 0];

    /**
     * Helper function for finding the score based on the distance and ranges
     * @param {Decimal} distance distance in KM
     * @returns {Decimal} Score
     */
    const findScoreFromRange = (distance) => { 
        for(var i = ranges.length - 1; i >= 0; i--) {
            if (distance <= ranges[i]) {
                return (i + 1) / 10;
            }
        }
    }

    return locations.map((loc) => {
        // If distance provided use in scoring
        if (loc.hasOwnProperty('distance')) {
            distanceScore = findScoreFromRange(loc.distance);
            // Assign score
            return { ...loc, score: distanceScore }
        } else {
            // Assign base score based on number of results since all are equally likely
            return { ...loc, score: Math.max((1 / locations.length).toFixed(1), 0.1) }
        }
    })
}

/**
 * Format output for response
 * @param {Array<Location>} locations 
 * @returns {Array<Object>} Array of objects with only the required fields
 */
function formatOutputForResponse(locations) {
    return locations.map((loc) => {
        const { score, name, latitude, longitude, country_code, admin1_code } = loc;
        return { name: `${name}, ${admin1_code}, ${country_code}`, latitude, longitude, score };
    });
}


module.exports = {
    /**
     * Return top 5 suggstions based on passed prefix
     * @param {String} suggestion The prefix of the location to search for
     * @param {Object} coordinates Optional coordinates to sort by distance
     * @returns 
     */
    getSuggestions(suggestion, coordinates = null) {
        // get first character of suggestion
        const firstChar = suggestion.charAt(0).toLowerCase();
        const trieForLetter = cache.get(firstChar);
        // If no tree exists return empty array
        if (!trieForLetter) {
            return [];
        }
        let results = trieForLetter.findWordsByPrefix(suggestion);

        // sort by distance if coordinates are provided
        if (coordinates.latitude && coordinates.longitude) {
            // Assign distance to each result
            results = assignDistancesToLocationsArray(results, coordinates);
            // sort by distance
            results.sort(sortByDistance);
        }


        // Create score, format, and return
        //return scoreLocations(results);
        return formatOutputForResponse(scoreLocations(results))
    }
}