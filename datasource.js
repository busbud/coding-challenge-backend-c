const fs = require('fs/promises')
const parse = require('csv-parse/lib/sync')

const City = require('./models/city')


const COUNTRIES = {
  'CA': 'Canada,',
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
      const raw = await fs.readFile('./data/cities_canada-usa.tsv')
      records = parse(raw, {
        delimiter: '\t',
        relax: true,
      })
    } catch (ex) {
      console.log('There was an error initializing data.')
      throw ex
    }

    this.cities = records.slice(1).map(r => {
      const city = new City()
      city.name = r[1]
      city.latitude = r[4]
      city.longitude = r[5]
      const countryCode = r[8]
      city.country = COUNTRIES[countryCode]
      city.state = countryCode === 'CA' ? PROVINCES[r[10]] : r[10]
      return city
    })
  }
}

module.exports = Datasource
