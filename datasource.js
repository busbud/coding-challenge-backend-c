const fs = require('fs/promises')

const utils = require('./utils')
const City = require('./models/city')


const COUNTRIES = {
  'CA': 'Canada',
  'US': 'United States',
}

const PROVINCES = {
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
}

class Datasource {
  async initialize() {

    let records

    try {
      const raw = await fs.readFile('./data/cities_canada-usa.tsv', { encoding: 'utf8' })
      records = raw.split('\n').map(line => line.split('\t'))
    } catch (ex) {
      console.log('There was an error initializing data.')
      throw ex
    }

    this.cities = records.slice(1).map(r => {
      if (!r[0]) return

      const city = new City()
      city.name = r[1]
      city.normalizedName = utils.normalizeString(r[2])
      city.latitude = Number(r[4])
      city.longitude = Number(r[5])
      const countryCode = r[8]
      city.country = COUNTRIES[countryCode]
      city.state = countryCode === 'CA' ? PROVINCES[r[10]] : r[10]
      city.population = Number(r[14])

      city.index = ` ${city.normalizedName}`

      if (city.isValid()) return city
    }).filter(city => city)
  }

  getCities() {
    return this.cities
  }
}

module.exports = Datasource
