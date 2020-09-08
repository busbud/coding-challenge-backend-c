// Source https://www.movable-type.co.uk/scripts/latlong.html

import { GlobeLocation } from '../types/core';

const EARTH_RADIUS = 6371e3; // In meters
const EARTH_MAXIMUM_DISTANCE = Math.PI * EARTH_RADIUS;

export function distanceBetweenTwoPoints(a: GlobeLocation, b: GlobeLocation): number {
  const deltaPhi = getRad(a.lat - b.lat);
  const deltaLambda = getRad(a.long - b.long);

  const haversine =
    Math.sin(deltaPhi / 2) * Math.sin(deltaLambda / 2) +
    Math.cos(getRad(a.lat)) * Math.cos(getRad(b.lat)) * Math.sin(deltaLambda / 2) * Math.sin(deltaLambda / 2);

  const angDistance = Math.asin(Math.sqrt(Math.abs(haversine)));

  return 2 * EARTH_RADIUS * angDistance;
}

export function getGeoScore(userLocation: GlobeLocation, cityLocation: GlobeLocation): number {
  return 1 - distanceBetweenTwoPoints(userLocation, cityLocation) / EARTH_MAXIMUM_DISTANCE;
}

function getRad(deg: number): number {
  return (deg * Math.PI) / 180;
}

export default {
  distanceBetweenTwoPoints,
  getGeoScore,
};
