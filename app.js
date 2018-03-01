const http = require('http')
const path = require('path')
const util = require('util')
const fs = require('fs')
const querystring = require('querystring')
const port = process.env.PORT || 2345

// some constants used to tune the suggestion engine
const MIN_CITY_SIZE = 5000 // we don't considered city with less than MIN_CITY_SIZE habitants
const MIN_SEARCH_INPUT_LENGTH = 3 // at least 3 char required to trigger a search
const MAX_DISTANCE_PENALTY = 0.5 // maximum penalty that can be given because of distance with user
const MAX_POPULATION_PENALTY = 0.5 // maximum penalty that can be given because of the size of the city
const DISTANCE_RADIUS_PENALTY = 500 // radius used to incease the distance penalty (in KM)

const stat = util.promisify(fs.stat)
const cities = []

// load data
async function loadData (fileName = 'cities_canada-usa.tsv') {
  const filePath = path.resolve(__dirname, 'data', fileName)
  const stats = await stat(filePath)

  if (!stats.isFile()) {
    console.log(`Cannot load file: ${filePath}, not a file`)
  } else {
    let lineReader = require('readline').createInterface({
      input: fs.createReadStream(filePath)
    })

    return new Promise((resolve, reject) => {
      lineReader.on('line', function (line) {
        const tokens = line.split('\t')
        cities.push({
          name: tokens[2],
          displayName: `${tokens[2]}, ${tokens[10]}, ${tokens[8] === 'US' ? 'USA' : 'Canada'}`,
          latitude: tokens[4],
          longitude: tokens[5],
          country: tokens[8],
          population: tokens[14] !== '' ? parseInt(tokens[14], 10) : null
        })
      })
      lineReader.on('close', resolve)
      lineReader.on('error', reject)
    })
  }
}

// matching function
function match (params) {
  let location
  if (params.latitude !== undefined && params.longitude !== undefined) {
    location = { latitude: parseFloat(params.latitude), longitude: parseFloat(params.longitude) }
  }

  function deg2rad (deg) {
    return deg * (Math.PI / 180)
  }

  function haversineDistance (start, end) {
    const R = 6371 // earth radius in KM
    const dLat = deg2rad(end.latitude - start.latitude)
    const dLon = deg2rad(end.longitude - start.longitude)
    const a = Math.sin(dLat / 2) * Math.sin(dLat / 2) +
      Math.cos(deg2rad(start.latitude)) * Math.cos(deg2rad(end.latitude)) *
      Math.sin(dLon / 2) * Math.sin(dLon / 2)
    const c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a))
    const d = R * c
    return d
  }

  // closest cities get lowest penalty
  function applyDistanceScoringRule (matchedCity) {
    if (location) {
      const penalty = Math.round(haversineDistance(location, matchedCity) / DISTANCE_RADIUS_PENALTY) / 10
      matchedCity.score = Math.max(0, Math.round((matchedCity.score - Math.min(penalty, MAX_DISTANCE_PENALTY)) * 10) / 10)
    }
    return matchedCity
  }

  // biggest cities get lowest penalty
  function applyPopulationScoringRule (matchedCity) {
    if (matchedCity.population) {
      const penalty = Math.round(MAX_POPULATION_PENALTY * MIN_CITY_SIZE / matchedCity.population * 10) / 10
      matchedCity.score = Math.max(0, Math.round((matchedCity.score - penalty) * 10) / 10)
    }
    return matchedCity
  }

  // we discard cities with less than MIN_CITY_SIZE habitants
  return cities.filter(city => city.population > MIN_CITY_SIZE)
    .filter(city => city.name.toLowerCase().indexOf(params.q.toLowerCase()) > -1)
    .map((city) => ({ ...city, score: 1 }))
    .map(applyDistanceScoringRule)
    .map(applyPopulationScoringRule)
    .map(({ displayName, longitude, latitude, population, score }) => ({ name: displayName, longitude, latitude, score }))
    .sort((a, b) => b.score - a.score)
}

// loading data
loadData()

// starting server
module.exports = http.createServer(function (req, res) {
  if (req.url.indexOf('/suggestions') === 0) {
    const queryParams = req.url.indexOf('?') ? querystring.parse(req.url.split('?')[1]) : {}

    if (queryParams.q.length < MIN_SEARCH_INPUT_LENGTH) {
      res.writeHead(413, { 'Content-Type': 'text/plain' })
      res.end('too much results, please provide at least 3 char to perform a search')
    } else {
      res.writeHead(200, { 'Content-Type': 'application/json' })
      res.end(JSON.stringify({
        suggestions: match(queryParams)
      }))
    }
  } else {
    res.writeHead(404, { 'Content-Type': 'text/plain' })
    res.end()
  }
}).listen(port)
