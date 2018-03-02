const http = require('http')
const path = require('path')
const util = require('util')
const fs = require('fs')
const querystring = require('querystring')
const RadixTrie = require('radix-trie-js')
const port = process.env.PORT || 2345
const stat = util.promisify(fs.stat)

// some constants used to tune the suggestion engine
const MIN_CITY_SIZE = 5000 // we don't considered city with less than MIN_CITY_SIZE habitants
const MIN_SEARCH_INPUT_LENGTH = 3 // at least 3 char required to trigger a search
const MAX_DISTANCE_PENALTY = 0.5 // maximum penalty that can be given because of distance with user
const MAX_POPULATION_PENALTY = 0.5 // maximum penalty that can be given because of the size of the city
const LONGEST_DISTANCE_CONSIDERED = 2000 // Longest distance considered which leads to MAX_DISTANCE_PENALTY (in KM)

const cities = [] // we keep every city in a table
const radixTrie = new RadixTrie() // we store the city name and a previously-declared-array index in a radix trie

// load data
async function loadData (fileName = 'cities_canada-usa.tsv') {
  const filePath = path.resolve(__dirname, 'data', fileName)
  const stats = await stat(filePath)

  if (!stats.isFile()) {
    console.log(`Cannot load file: ${filePath}, not a file`)
  } else {
    // read file line by line
    let lineReader = require('readline').createInterface({
      input: fs.createReadStream(filePath)
    })

    // parse it to extract relevant data
    return new Promise((resolve, reject) => {
      lineReader.on('line', function (line) {
        const tokens = line.split('\t')
        const city = {
          name: tokens[2],
          displayName: `${tokens[2]}, ${tokens[10]}, ${tokens[8] === 'US' ? 'USA' : 'Canada'}`,
          latitude: tokens[4],
          longitude: tokens[5],
          country: tokens[8],
          population: tokens[14] !== '' ? parseInt(tokens[14], 10) : null
        }
        cities.push(city)
        radixTrie.add(city.name, cities.length - 1)
      })
      lineReader.on('close', resolve)
      lineReader.on('error', reject)
    })
  }
}

// util fn
function deg2rad (deg) {
  return deg * (Math.PI / 180)
}

// util fn to get the distance between 2 geo coordinates
// KUDOS to SO - https://stackoverflow.com/questions/14560999/using-the-haversine-formula-in-javascript
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
function applyDistanceScoringRule (city, location) {
  if (location) {
    const penalty = Math.round(MAX_DISTANCE_PENALTY * Math.min(haversineDistance(location, city) / LONGEST_DISTANCE_CONSIDERED, 1) * 10) / 10
    city.score = Math.max(0, Math.round((city.score - penalty) * 10) / 10)
  }
  return city
}

// biggest cities get lowest penalty
function applyPopulationScoringRule (city) {
  if (city.population) {
    const penalty = Math.round(MAX_POPULATION_PENALTY * MIN_CITY_SIZE / city.population * 10) / 10
    city.score = Math.max(0, Math.round((city.score - penalty) * 10) / 10)
  }
  return city
}

// this is our main match fn
function matchAgainstRadixTrie (params) {
  let location
  if (params.latitude !== undefined && params.longitude !== undefined) {
    location = { latitude: parseFloat(params.latitude), longitude: parseFloat(params.longitude) }
  }

  const res = radixTrie.fuzzyGet(params.q)
  return Array.from(res)
    .map((match) => cities[match[1]])
    .filter(city => city.population > MIN_CITY_SIZE)
    .map((city) => ({ ...city, score: 1 }))
    .map((city) => location ? applyDistanceScoringRule(city, location) : city)
    .map(applyPopulationScoringRule)
    .map(({ displayName, longitude, latitude, population, score }) => ({ name: displayName, longitude, latitude, score }))
    .sort((a, b) => b.score - a.score)
}

// loading datas
loadData()

// starting server
module.exports = http.createServer(function (req, res) {
  if (req.url.indexOf('/suggestions') === 0) {
    const queryParams = req.url.indexOf('?') ? querystring.parse(req.url.split('?')[1]) : {}

    if (queryParams.q === undefined || queryParams.q.length < MIN_SEARCH_INPUT_LENGTH) {
      res.writeHead(413, { 'Content-Type': 'text/plain' })
      res.end('too much results, please provide at least 3 char to perform a search')
    } else {
      res.writeHead(200, { 'Content-Type': 'application/json' })
      res.end(JSON.stringify({
        suggestions: matchAgainstRadixTrie(queryParams)
      }))
    }
  } else {
    res.writeHead(404, { 'Content-Type': 'text/plain' })
    res.end()
  }
}).listen(port)
console.log('server started!')
