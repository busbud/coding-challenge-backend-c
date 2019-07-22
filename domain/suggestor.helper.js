//  Application Code
const suggestionConfig = require('../config').suggestionConfig;

/**
 * Search fors pattern within str. Returns founds true when a full or partial match is found
 * confidence is between 0..1 where 1 is exact match
 * @param   {string}  str                 The string we are going to search,
 * @param   {string}  pattern             The pattern that will be search within the target string
 * @return  {Object}  result              Object that contains search result and confidence score
 * @return  {boolean} result.found        True when pattern found, False when pattern not found
 * @return  {number}  result.confidence   Number between 0 and 1, where 1 is exact match
 */
function searchString(str, pattern) {
  // validate str 7 pattern inputs
  var has_str = (str && (str.length > 0));
  var has_pattern = (pattern && (pattern.length > 0));
  if (!(has_pattern && has_str)) {
    return { found: false };
  }
  // clean strings
  str = cleanAndNormalizeString(str);
  pattern = cleanAndNormalizeString(pattern);
  // look for pattern in string
  var found = (str.indexOf(pattern) === 0);
  if (!found) {
    return { found: false };
  }
  // the confidence of the match is based on the on long the pattern is versus the string you are search
  // an exact match would be 1, since they would be the same length
  var confidence = (pattern.length / str.length);
  return { found: true, confidence: confidence };
}

/**
 * Returns the score of a city based on Search Query and Search Coordinate
 * Score is between 0 and 1 (inclusive) indicating confidence in the city relative
 * to search criterion (1 is most confident)
 * @param   {Object}  city                          City to be scored
 * @param   {string}  search_term                   Search string used to score cities
 * @param   {Object}  search_coordinate             Search Coordinate used to score cities
 * @param   {number}  search_coordinate.latitude    Search Coordinate's latitude
 * @param   {number}  search_coordinate.longitude   Search Coordinate's longitude
 * @return  {number}  score                         Value between 0 and 1 where 1 is the most confident
 */
function scoreCity(city, search_term, search_coordinate) {
  //  set the name and distance score weights to their default value
  let nameScoreWeight = 1.0;
  let distanceScoreWeight = 0.0;
  if (search_coordinate) {
    // is a search coordinate is specified modify the score weights appropriately
    distanceScoreWeight = suggestionConfig.coordinateScoreWeight;
    nameScoreWeight = (1.0 - distanceScoreWeight);
  }
  //  get the name search score
  const scoreByName = searchString(city.ascii, search_term).confidence;
  //  get the distance score
  const scoreByDistance = (search_coordinate ? normalizedDistanceBetweenCoordinates(city.coordinate, search_coordinate) : 1);
  //  normalize the score based on the specified weights
  let score = ((scoreByName * nameScoreWeight) + (scoreByDistance * distanceScoreWeight));
  //  round score number to desired precision
  score = Number(score).toFixed(suggestionConfig.scorePrecision || 1);
  return score;
}

/**
 * This uses the ‘haversine’ formula to calculate the great-circle distance between two points – that is,
 * the shortest distance over the earth’s surface – giving an ‘as-the-crow-flies’
 * distance between the points (ignoring any hills they fly over, of course!).
 * @param   {Object}  coordinate_a            First coordinate point
 * @param   {number}  coordinate_a.latitude   First coordinate's latitude
 * @param   {number}  coordinate_a.longitude  First coordinate's longitude
 * @param   {Object}  coordinate_b            Second coordinate point
 * @param   {number}  coordinate_b.latitude   Second coordinate's latitude
 * @param   {number}  coordinate_b.longitude  Second coordinate's longitude
 * @return  {number}  distance                Distance between both coordinates in kilometers
 */
function distanceBetweenCoordinates(coordinate_a, coordinate_b) {
  const EARTH_RADIUS = 6371; // earth radius in kilometers
  const phi_1 = toRadians(coordinate_a.latitude);
  const phi_2 = toRadians(coordinate_b.latitude);
  const delta_phi = toRadians(coordinate_b.latitude - coordinate_a.latitude);
  const delta_lambda = toRadians(coordinate_b.longitude - coordinate_a.longitude);
  const a = Math.sin(delta_phi / 2) * Math.sin(delta_phi / 2) +
    Math.cos(phi_1) * Math.cos(phi_2) *
    Math.sin(delta_lambda / 2) * Math.sin(delta_lambda / 2);
  const c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));
  const d = EARTH_RADIUS * c;
  return d;
}

/**
 * This uses the ‘haversine’ formula to calculate the great-circle distance between two points and normalizes
 * that distance according to the earth's circumference
 * The maximum distance between 2 points would be the earth circumference divided by two. In other words the distance
 * between two points cannot exceed that value. We want to return a value between 0 and 1 inclusively. As the distance between
 * both points get smaller the returned value should appraoch 1.
 * @param   {Object}  coordinate_a            First coordinate point
 * @param   {number}  coordinate_a.latitude   First coordinate's latitude
 * @param   {number}  coordinate_a.longitude  First coordinate's longitude
 * @param   {Object}  coordinate_b            Second coordinate point
 * @param   {number}  coordinate_b.latitude   Second coordinate's latitude
 * @param   {number}  coordinate_b.longitude  Second coordinate's longitude
 * @return  {number}  distance                Normalize value between 0 and 1 between both coodinates
 *                                            0: Distance between the points are the biggest possible value (i.e. 20037.5 KM)
 *                                            1: Distance between both points are the smallest possible value (i.e 0 KM)
 */
function normalizedDistanceBetweenCoordinates(coordinate_a, coordinate_b) {
  const EARTH_CIRCUMFERENCE = 40075.0; // earth circumference in kilometers
  const HALF_EARTH_CIRCUMFERENCE = EARTH_CIRCUMFERENCE / 2.0; // half earth circumference in kilometers
  var distanceInKm = distanceBetweenCoordinates(coordinate_a, coordinate_b);
  return ((HALF_EARTH_CIRCUMFERENCE - distanceInKm) / HALF_EARTH_CIRCUMFERENCE);
}

/**
 * Cleans and normalizes a string
 * To remove accents used solution found here:
 * https://stackoverflow.com/questions/990904/remove-accents-diacritics-in-a-string-in-javascript
 * TODO: Consider using library to remove accents. Such as:
 *              - https://github.com/tyxla/remove-accents,
 *              - https://www.npmjs.com/package/diacritics
 *              - https://www.npmjs.com/package/diacritic
 * @param   {string}  string  String which will be cleand and normalized
 * @return  {string}  string  Normalized and cleaned string
 */
function cleanAndNormalizeString(string) {
  string = string.toLowerCase()
    .normalize('NFD')
    .replace(/[\u0300-\u036f]/g, '');
  return string;
}
const toRadians = (number) => { return number * Math.PI / 180; };

module.exports = {
  searchString: searchString,
  scoreCity: scoreCity,
  distanceBetweenCoordinates: distanceBetweenCoordinates,
  normalizedDistanceBetweenCoordinates: normalizedDistanceBetweenCoordinates,
  cleanAndNormalizeString: cleanAndNormalizeString
};
