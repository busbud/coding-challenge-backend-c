const fs = require('fs')
const getCitiesFromString = require('./getCitiesFromString.js')
const suggest = require('./suggest.js')
const createServer = require('./createServer.js')
const port = process.env.PORT || 2345;
const TSV_PATH = './data/cities_canada-usa.tsv'

const citiesString = fs.readFileSync(TSV_PATH, {encoding: 'utf-8'})
const cities = getCitiesFromString(citiesString)

module.exports = createServer({cities, suggest}, {port})