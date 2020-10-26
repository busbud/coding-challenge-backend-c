const expect = require('chai').expect
const sinon = require('sinon')

const Services = require('../services')
const Datasource = require('../datasource')

afterEach(() => {
  sinon.restore()
})

const getOpts = fixture => ({
  admin2Codes: `${__dirname}/../data/admin2Codes.txt`,
  cities: `${__dirname}/fixtures/${fixture}.tsv`,
})

describe.only('Datasource initialization', () => {
  it('should exclude any city outside of CA & US', async () => {
    const ds = new Datasource()
    await ds.initialize(getOpts('cities_world'))
    expect(ds.cities).to.exist
    expect(ds.cities).to.be.instanceOf(Array)
    expect(ds.cities.length).to.equal(2)
    expect(ds.cities.map(city => city.countryCode)).to.not.include('AR')
  })

  it('should exclude any city with population below 5000', async () => {
    const ds = new Datasource()
    await ds.initialize(getOpts('cities_small'))
    expect(ds.cities).to.exist
    expect(ds.cities).to.be.instanceOf(Array)
    expect(ds.cities.filter(city => city.population < 5000)).to.be.empty
  })

  it('should exclude duplicate cities that cannot be disambiguated', async () => {
    const ds = new Datasource()
    await ds.initialize(getOpts('cities_duplicates'))
    expect(ds.cities).to.exist
    expect(ds.cities).to.be.instanceOf(Array)
    expect(ds.cities.length).to.equal(1)
  })

  it('should make city names unique when possible', async () => {
    const ds = new Datasource()
    await ds.initialize(getOpts('cities_duplicates_fixable'))
    expect(ds.cities).to.exist
    expect(ds.cities).to.be.instanceOf(Array)
    expect(ds.cities.length).to.equal(2)
    expect(ds.cities[0].getDisplayName()).to.equal('Fairwood (King County), WA, United States')
    expect(ds.cities[1].getDisplayName()).to.equal('Fairwood (Spokane County), WA, United States')
  })
})
