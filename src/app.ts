import levenshtein from 'js-levenshtein';
import cities from '../data/cities_canada-usa.json';
import { City } from './types';

/**
 * queryDataSet.
 *
 * @description Filters dataset based on given query and location
 * then evaluates all filtered results
 *
 * @param {string} q
 * @param {number | null} latitude
 * @param {number | null} longitude
 */
export function queryDataSet(q: string, latitude: number | null, longitude: number | null) {
  let filteredCities = cities.filter((city: any) => city.name.toLowerCase().includes(q.toLowerCase()));
  if (filteredCities.length) {
    let scoredCities: City[] = scoreWordDiffrence(filteredCities, q);
    if (latitude && longitude) {
      scoredCities = scoreDistance(scoredCities, longitude, latitude);
    }
    return sumCityScores(scoredCities);
  } else {
    return [];
  }
}

/**
 * sumCityScores.
 *
 * @description Calculates scored based on summary of 
 * location distance and word diffrence
 *
 * @param {City[]} scoredCities
 */
function sumCityScores(scoredCities: City[]) {
  let result;
  if ('distance' in scoredCities[0]) {
    result = scoredCities.map(city => {
      city.score = ((city.levenshtein || 0) + (city.distance || 0))/2.0 || 0;
      delete city.distance;
      delete city.levenshtein;
      return city;
    });
  } else {
    result = scoredCities.map(city => {
      city.score = city.levenshtein;
      delete city.levenshtein;
      return city;
    });
  }
  // Sort by score
  return result.sort((a: City, b: City) => (b.score || 0) - (a.score || 0));
}

/**
 * scoreWordDiffrence.
 *
 * @description Calculates inverse normalization of all word differences
 * Uses levenshtein distance to calculate difference 
 *
 * @param {City[]} filteredCities
 * @param {string} q
 */
function scoreWordDiffrence(filteredCities: City[], q: string) {
  let maxDiffrence = 0.0;
  let minDiffrence = Number.MAX_VALUE;
  let scoredCities = filteredCities.map(city => {
    // Calculate word difference
    const diffrence = levenshtein(city.name.toLowerCase(), q.toLowerCase());
    maxDiffrence = (maxDiffrence < diffrence) ? diffrence : maxDiffrence;
    minDiffrence = (minDiffrence > diffrence) ? diffrence : minDiffrence;
    return {
      ...city,
      levenshtein: diffrence
    }
  })
  // Normalize to 0-1
  scoredCities = scoredCities.map(city => {
    let levenshtein = (maxDiffrence - city.levenshtein)/(maxDiffrence - minDiffrence)
    return {
      ...city,
      levenshtein
    }
  })
  return scoredCities;
}

/**
 * scoreDistance.
 *
 * @description Calculates inverse normalization of all location distances
 *
 * @param {City[]} filteredCities
 * @param {number} longitude
 * @param {number} latitude
 */
function scoreDistance(filteredCities: City[], longitude: number, latitude: number) {
  let maxDistance = 0.0;
  let minDistance = Number.MAX_VALUE;
  let scoredCities = filteredCities.map(city => {
    // Use distance between points
    const distance = distanceBetweenPoints(longitude, latitude, city.longitude, city.latitude);
    maxDistance = (maxDistance < distance) ? distance : maxDistance;
    minDistance = (minDistance > distance) ? distance : minDistance;
    return {
      ...city,
      distance
    }
  });

  // Normalise to 0-1
  scoredCities = scoredCities.map(city => {
    let distance = (maxDistance - city.distance)/(maxDistance - minDistance)
    return {
      ...city,
      distance
    }
  })
  return scoredCities;
}

/**
 * distanceBetweenPoints.
 *
 * @description Calculates distance between coordinates
 *
 * @param {number} x1
 * @param {number} y1
 * @param {number} x2
 * @param {number} y2
 */
function distanceBetweenPoints(x1: number, y1: number, x2: number, y2: number) {
  return Math.sqrt((x2 - x1) * 2 + (y2 - y1) * 2);
}

/**
 * formatPosition.
 *
 * @description formatting coordinates query parameters as floats
 *
 * @param {string | null} point
 */
export function formatPosition(point: string | null) {
  return (point && !isNaN(parseFloat(point))) ? parseFloat(point) : null;
}

/**
 * parseUrl.
 *
 * @description URL parsing with exception handling
 *
 * @param {string} url
 */
export function parseUrl(url: string) {
  try {
    return new URL(url, `http://localhost`);
  } catch(e) {
    console.error(e);
    return null;
  }
}
