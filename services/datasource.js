const fs = require('fs/promises')
const flatten = require('lodash/flatten')

const utils = require('./utils')
const City = require('../models/city')

const DATA_DIRECTORY = `${__dirname}/../data`

// Only cities from countries included here will be processed
// The optional `states` attribute is used to map from state/province codes to names
const COUNTRIES = {
  'CA': {
    name: 'Canada',
    states: {
      '01': 'AB',
      '02': 'BC',
      '03': 'MB',
      '04': 'NB',
      '05': 'NL',
      '06': '',
      '07': 'NS',
      '08': 'ON',
      '09': 'PE',
      '10': 'QC',
      '11': 'SK',
      '12': 'YT',
      '13': 'NT',
      '14': 'NU',
    },
  },
  'US': {
    name: 'United States',
    states: null
  },
}

class Datasource {
  async parseAdmin2Codes(opts) {
    const raw = await fs.readFile(opts.admin2Codes ?? `${DATA_DIRECTORY}/admin2Codes.txt`, { encoding: 'utf8' })
    const records = raw.split('\n').map(line => line.split('\t'))

    this.admin2 = records.reduce((memo, curr) => {
      const [country, ...rest] = curr[0].split('.')
      if (!COUNTRIES[country]) return memo
      memo[curr[0]] = curr[1]
      return memo
    }, {})
  }

  async parseCities(opts) {
    const raw = await fs.readFile(opts.cities ?? `${DATA_DIRECTORY}/cities_canada-usa.tsv`, { encoding: 'utf8' })
    const records = raw.split('\n').map(line => line.split('\t'))

    this.cities = []
    records.slice(1).forEach(r => {
      const city = new City()

      city.id = r[0]
      if (!city.id) return

      city.name = r[1]
      city.asciiName = r[2]
      city.alternateNames = r[3]
      city.latitude = Number(r[4])
      city.longitude = Number(r[5])

      city.countryCode = r[8]
      const countryDef = COUNTRIES[city.countryCode]
      if (!countryDef) return

      city.countryName = countryDef.name
      city.admin1 = r[10]
      city.state = countryDef.states
        ? countryDef.states[city.admin1]
        : city.admin1
      city.admin2 = r[11]
      city.admin2Name = this.admin2[city.getAdmin2Key()]

      city.population = Number(r[14])
      if (isNaN(city.population) || city.population < 5000) return

      if (city.isValid()) {
        this.cities.push(city)
      }
    })
  }

  fixDuplicates() {
    const getDuplicatesBy = fn => {
      const groups = []
      this.cities.forEach(city => {
        const key = fn(city)
        if (!groups[key]) {
          groups[key] = { count: 0, cities: [] }
        }
        groups[key].count++
        groups[key].cities.push(city)
      })
      return Object.values(groups).filter(x => x.count > 1)
    }

    // Find records that cannot be disambiguated
    let duplicates = getDuplicatesBy(city => `${city.getDisplayName()}|${city.admin2}`)

    // For each group keep the first occurrence
    // TODO: improve this by keeping the one with higher population or -alternatively- have a disambiguation list
    let remove = {}
    duplicates.forEach(dup => {
      dup.cities.slice(1).forEach(city => {
        remove[city.id] = city
      })
    })

    this.cities = this.cities.filter(city => !remove[city.id])

    if (Object.keys(remove) > 0) {
      console.log('The following cities were duplicated and could not be disambiguated:')
      Object.values(remove).forEach(city => {
        console.log(`ID: ${city.id} - ${city.getDisplayName()} (population ${city.population})`)
      })
    }


    // Find duplicate records that can be disambiguated and fix them
    duplicates = getDuplicatesBy(city => city.getDisplayName())

    // For each of these cities, add the admin2 description to their name
    const ambiguous = flatten(duplicates.map(dup => dup.cities))
    ambiguous.forEach(city => {
      city.name = `${city.name} (${city.admin2Name})`
    })
  }

  indexCities() {
    this.cities.forEach(city => {
      city.normalizedName = utils.normalizeString(city.asciiName)
      city.index = ` ${city.normalizedName}`
    })
  }

  async initialize(opts = {}) {
    try {
      await this.parseAdmin2Codes(opts)
      await this.parseCities(opts)
    } catch (ex) {
      console.error('There was an error reading data files.')
      throw ex
    }

    this.fixDuplicates()
    this.indexCities()
  }

  getCities() {
    return this.cities
  }
}

module.exports = Datasource
