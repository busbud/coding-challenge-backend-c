'use strict';

/**
 * Score the city population.
 *
 * The most populated city will have a score of 1, while the least populated
 * one will have a score of 0.
 */
function population(cities) {
  const populations = cities.map(c => c.population);
  const min = Math.min(...populations);
  const max = Math.max(...populations);
  const size = max - min;

  return city => (city.population - min) / size;
}

export default (_, items) =>
  population(items);
