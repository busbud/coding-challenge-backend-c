const expect = require('chai').expect

const Services = require('../services/services')
const Datasource = require('../services/datasource')

afterEach(() => {
})

const getOpts = fixture => ({
  admin2Codes: `${__dirname}/fixtures/admin2Codes.txt`,
  cities: `${__dirname}/fixtures/${fixture}.tsv`,
})

describe('Datasource initialization', () => {
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

describe('Get suggestions', () => {
  let services
  before(async () => {
    const ds = new Datasource()
    await ds.initialize(getOpts('cities_canada-usa'))
    services = new Services(ds)
  })
  it('should find matches based on any word (for multi-word city names)', () => {
    const results = services.getSuggestions('York')
    expect(results.map(city => city.name)).to.include('New York City, NY, United States')
  })
  it('should rank exact match better than partial match', () => {
    const results = services.getSuggestions('York')
    const York = results.find(city => city.name.startsWith('York'))
    const NewYork = results.find(city => city.name.startsWith('New York City'))
    expect(York).to.exist
    expect(NewYork).to.exist
    expect(York.score).to.be.greaterThan(NewYork.score)
  })
  it('should improve score based on latitude & longitude', () => {
    let NewYork
    // New York City location is lat: 40.71427, lon: -74.00597
    let results = services.getSuggestions('New Yor', 0, 0)
    NewYork = results.find(city => city.name.startsWith('New York City'))
    expect(NewYork).to.exist
    const scoreFromAfar = NewYork.score
    console.log(scoreFromAfar)

    results = services.getSuggestions('New Yor', 40, -74)
    NewYork = results.find(city => city.name.startsWith('New York City'))
    expect(NewYork).to.exist
    const scoreFromNear = NewYork.score
    console.log(scoreFromNear)

    expect(scoreFromNear).to.be.greaterThan(scoreFromAfar)
  })
  it('should improve score based on population', () => {
    // London, ON, Canada has a population of around 350k
    // London, OH, United States has a population below 10k
    let results = services.getSuggestions('London')
    const LondonONCanada = results.find(city => city.name === 'London, ON, Canada')
    const LondonOHUS = results.find(city => city.name === 'London, OH, United States')
    expect(LondonONCanada).to.exist
    expect(LondonOHUS).to.exist
    expect(LondonONCanada.score).to.be.greaterThan(LondonOHUS.score)
  })
})