const http = require('http')
const path = require('path')
const util = require('util')
const fs = require('fs')
const querystring = require('querystring')
const port = process.env.PORT || 2345

const stat = util.promisify(fs.stat)
const cities = []

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
          population: tokens[15] !== '' ? parseInt(tokens[15], 10) : null
        })
      })
      lineReader.on('close', resolve)
      lineReader.on('error', reject)
    })
  }
}

function match (params) {
  let location
  if (params.latitude !== undefined && params.longitude !== undefined) {
    location = { latitude: parseFloat(params.latitude), longitude: parseFloat(params.longitude) };
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

  function applyDistanceScoringRule (matchedCity) {
    if (location) {
      const maxDistancePenalty = 0.8 // max penalty applied with distance
      const penaltyStepDistance = 500 // increase penalty each *penaltyStepDistance* KM
      const penalty = Math.round(haversineDistance(location, matchedCity) / penaltyStepDistance) / 10
      matchedCity.score = Math.max(0, Math.round((matchedCity.score - Math.min(penalty, maxDistancePenalty)) * 10) / 10)
    }
    return matchedCity
  }

  function applyPopulationScoringRule (matchedCity) {
    if (matchedCity.population && matchedCity.population < 5000) {
      matchedCity.score = 0.1
    }
    return matchedCity
  }

  return cities.filter(city => city.name.indexOf(params.q) > -1)
    .map(({ displayName, longitude, latitude }) => ({ displayName, longitude, latitude, score: 1 }))
    .map(applyDistanceScoringRule)
    .map(applyPopulationScoringRule)
    .sort((a, b) => a.score < b.score)
}

(async function start () {
  await loadData()
  console.log(`loaded ${cities.length} cities`)
  console.log(`loaded ${cities.filter(c => c.population > 5000).length} cities with more than 5000 living souls`)

  http.createServer(function (req, res) {
    if (req.url.indexOf('/suggestions') === 0) {
      const queryParams = req.url.indexOf('?') ? querystring.parse(req.url.split('?')[1]) : {}
      res.writeHead(200, { 'Content-Type': 'application/json' })
      res.end(JSON.stringify({
        suggestions: match(queryParams)
      }))
    } else {
      res.writeHead(404, { 'Content-Type': 'text/plain' })
      res.end()
    }
  }).listen(port, '127.0.0.1')

  console.log('Server running at http://127.0.0.1:%d/suggestions', port)
})()
