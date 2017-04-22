/* eslint-disable no-console */
/* eslint-disable import/no-extraneous-dependencies */
const sphereKnn = require('sphere-knn');
const geolib = require('geolib');

// Reference data
const referenceData = [
  { latitude: -1, longitude: -1 },
  { latitude: -27, longitude: -32 },
  { latitude: -90, longitude: -180 },
  { latitude: 90, longitude: -180 },
  { latitude: 45, longitude: 67 },
  { latitude: 76, longitude: 75 },
  { latitude: 76, longitude: 76 },
  { latitude: 76, longitude: 80 },
  { latitude: 24, longitude: 123 },
  { latitude: 90, longitude: -180 }, // duplicate
  { latitude: 54.26684, longitude: -110.73505 },
  { latitude: 23.25321, longitude: 121.78402 },
];

// Lookup this point
const pointLat = 45.66678;
const pointLon = -73.88249;

// -----------------------------

// Results sphere-knn
const lookup = sphereKnn(referenceData);
const neighborsSphereKnn = lookup(pointLat, pointLon, 10);
console.log('sphere-knn Neighbors: ', neighborsSphereKnn);

// -----------------------------

// Results geolib
const neighborsGeolib = geolib.orderByDistance(
  { latitude: pointLat, longitude: pointLon }, referenceData);
console.log('geolib Neighbors: ', neighborsGeolib);

