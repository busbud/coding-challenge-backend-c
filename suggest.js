const normalize = (string) =>
  string
  .normalize('NFD')
  .replace(/[\u0300-\u036f]/g, "")
  .toLowerCase()

// to be sure that score is between 0 and 1
const normalizeScore = score => {
  if (score > 1) return 1
  if (score < 0) return 0
  return score
}

const deg2rad = (deg) => deg * (Math.PI/180)

// based on Haversine formula : https://stackoverflow.com/questions/27928/calculate-distance-between-two-latitude-longitude-points-haversine-formula
const getDistanceFromLatLonInKm = (lat1,lon1,lat2,lon2) => {
  const earthRadius = 6371; // in km
  const dLat = deg2rad(lat2-lat1);  // deg2rad below
  const dLon = deg2rad(lon2-lon1); 
  const a = 
    Math.sin(dLat/2) * Math.sin(dLat/2) +
    Math.cos(deg2rad(lat1)) * Math.cos(deg2rad(lat2)) * 
    Math.sin(dLon/2) * Math.sin(dLon/2)
    ; 
  const c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1-a)); 
  const distance = earthRadius * c; // Distance in km
  return distance;
}

const calculateScoreFromName = (city, normalizedSearch) => {
  const score = 1 - (city.asciiname.length - normalizedSearch.length) * 0.1
  return normalizeScore(score)
}

const calculateScoreFromDistance = (lat1,lon1,lat2,lon2) => {
  // earth radius is roughly the max distance between two points
  // for now we just make a simple linear fonction
  const score = 1 - getDistanceFromLatLonInKm(lat1,lon1,lat2,lon2) / 6371 * 2
  return normalizeScore(score)
}

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

function suggest(cities, search, lat, lon) {
  const normalizedSearch = normalize(search)
  return cities
    .filter(city =>
      normalize(city.asciiname).indexOf(normalizedSearch) == 0
    )
    .map(city => 
      Object.assign(city, {score: calculateScore(city, normalizedSearch, lat, lon)})
    )
    .sort((a,b) => b.score - a.score)
}

module.exports = suggest