'use strict';

import {getDistance} from 'geolib';

/**
 * Score the city position relative to given coordinates.
 *
 * The closest city will have a score of 1 while the farthest one will have a
 * score of 0.
 */
function distance(coords, cities) {
  // Build a weak map of city distances from given coordinates
  const map = cities.reduce(
    (map, c) => map.set(c, getDistance(coords, c)),
    new WeakMap()
  );

  const distances = cities.map(::map.get);
  const min = Math.min(...distances);
  const max = Math.max(...distances);
  const size = max - min;

  return city => (max - map.get(city)) / size;
}

export default (query, items) => {
  if (!(query.latitude && query.longitude)) {
    return () => 0;
  }

  return distance(query, items);
};
