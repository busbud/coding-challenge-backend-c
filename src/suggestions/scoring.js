const geolib = require('geolib');

const earthDiameter = 12742018;  // Value in metres (m)


function computeGeoScore(distance) {
  return (1 - ((distance - 0) / (earthDiameter - 0)));
}

function computeNameScore(nameQuery, nameCity) {
  return nameQuery.length !== 0 ? (1 / (nameCity.length / nameQuery.length)) : null;
}

/**
 * Function tasked with augmenting City objects with relevancy scores in terms of
 * string likeness for the 'q' query parameter and in terms of proximity for the
 * coordinates query parameters.
 *
 * Sorts and limits the results.
 */
export default function scoreCitiesByRelevancy(cities, nameQuery, lat, lon, maxResults = 5) {
  return new Promise((resolve) => {
    if (lat !== null && lon !== null) {
      // Compute distances between each possibly relevant city and given coordinates, if applicable
      const cityDistances = geolib.orderByDistance({ latitude: lat, longitude: lon }, cities);

      // Compute a normalized score for each city based on the distance discovered
      cityDistances.forEach((cityDistance) => {
        // 'key' refers to the index of the cities Array
        const city = cities[Number.parseInt(cityDistance.key, 10)];
        city.geoScore = computeGeoScore(cityDistance.distance);
      });
    }

    // Compute a normalized score for each city based on the string likeness of the city name
    // Since all the names we will have come from a filtered results of querying the database
    // with a regex for how the city name should begin, the formula used here can be simple.
    for (let i = 0; i < cities.length; i += 1) {
      const city = cities[i];
      city.nameScore = computeNameScore(nameQuery, city.name);
    }

    // Sort by 'final' score and return results up to the requested max
    cities.sort((a, b) => b.totalScore - a.totalScore);
    resolve(cities.slice(0, maxResults));
  });
}
