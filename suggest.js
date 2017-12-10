
/**
 * Removes accents and diatrics of a string
 * https://stackoverflow.com/questions/990904/remove-accents-diacritics-in-a-string-in-javascript
 * 
 * @param {String} string
 * @returns {String}
 */
const normalizeString = (string) =>
  string
  .normalize('NFD')
  .replace(/[\u0300-\u036f]/g, "")
  .toLowerCase()

/**
 * Returns a score between 1 and 0
 * 
 * @param {Number} score 
 * @returns {Number}
 */
const normalizeScore = score => {
  if (score > 1) return 1
  if (score < 0) return 0
  return score
}

/**
 * Returns the distance in km between two points
 * based on Haversine formula : https://stackoverflow.com/questions/27928/calculate-distance-between-two-latitude-longitude-points-haversine-formula
 * 
 * @param {Number} lat1 
 * @param {Number} lon1 
 * @param {Number} lat2 
 * @param {Number} lon2 
 * @returns {Number}
 */
const getDistanceFromLatLonInKm = (lat1,lon1,lat2,lon2) => {
  const deg2rad = (deg) => deg * (Math.PI/180)
  const earthRadius = 6371; // in km
  const dLat = deg2rad(lat2-lat1)
  const dLon = deg2rad(lon2-lon1) 
  const a = 
    Math.sin(dLat/2) * Math.sin(dLat/2) +
    Math.cos(deg2rad(lat1)) * Math.cos(deg2rad(lat2)) * 
    Math.sin(dLon/2) * Math.sin(dLon/2)
  const c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1-a))
  const distance = earthRadius * c // Distance in km
  return distance
}

/**
 * Calculates a score based on string length
 * more the city has a long name, smaller is the score
 * 
 * @param {Object} city 
 * @param {String} normalizedSearch 
 * @returns {Number}
 */
const calculateScoreFromName = (city, normalizedSearch) => {
  const score = 1 - (city.asciiname.length - normalizedSearch.length) * 0.1
  return normalizeScore(score)
}
/**
 * Calculates a score based on distance
 * further is the city, smaller is the score
 * 
 * @param {Number} lat1 
 * @param {Number} lon1 
 * @param {Numberany} lat2 
 * @param {Numberany} lon2 
 * @returns {Number}
 */
const calculateScoreFromDistance = (lat1,lon1,lat2,lon2) => {
  // earth radius * 2 is roughly the max distance between two points
  // for now we just make a simple linear fonction
  const score = 1 - getDistanceFromLatLonInKm(lat1,lon1,lat2,lon2) / 6371 * 2
  return normalizeScore(score)
}

/**
 * Calculates the score of a city based on the search, the latitude and the longitude
 * if latitude and longitude are given
 *    calculates a score based on the distance and the city name
 * else
 *  calculates a score based on the city name
 * 
 * @param {Object} city 
 * @param {String} normalizedSearch 
 * @param {Number} lat 
 * @param {Number} lon 
 * @returns {Number} a score between 0 and 1
 */
const calculateScore = (city, normalizedSearch, lat, lon) => {
  let score
  if (lat && lon) {
    const nameWeight = 1
    const distanceWeight = 1
    score = (
      calculateScoreFromName(city, normalizedSearch) * nameWeight
      + calculateScoreFromDistance(city.latitude, city.longitude, lat, lon) * distanceWeight
    ) / (nameWeight + distanceWeight)
  } else {
    score = calculateScoreFromName(city, normalizedSearch)
  }
  return normalizeScore(score)
}

/**
 * Simple suggestion algorithm
 * 
 * 1. it filters the city that has the beginging of its normalized name 
 *    matching the normalized search
 * 2. it adds a score to each result
 * 3. it sorts by descending score
 * 
 * @param {Object[]} cities 
 * @param {String} search 
 * @param {Number} lat 
 * @param {Number} lon 
 * @returns {Object[]}
 */
function suggest(cities, search, lat, lon) {
  const normalizedSearch = normalizeString(search)
  return cities
    .filter(city =>
      normalizeString(city.asciiname).indexOf(normalizedSearch) == 0
    )
    .map(city => 
      Object.assign(city, {score: calculateScore(city, normalizedSearch, lat, lon)})
    )
    .sort((a,b) => b.score - a.score)
}

module.exports = suggest