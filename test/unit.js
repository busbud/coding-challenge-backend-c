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
  it('should find matches containing all terms in the search string', () => {
    const results = services.getSuggestions('mon ou')
    const cityNames = results.map(city => city.name)
    expect(cityNames).to.include('Montréal-Ouest, QC, Canada')
    expect(cityNames).to.not.include('Montréal, QC, Canada')
    expect(cityNames).to.not.include('Monmouth, IL, United States')
  })
  it('should always match start of name', () => {
    const results = services.getSuggestions('York')
    expect(results.map(city => city.name)).to.not.include('New York City, NY, United States')
  })
  it('should rank exact match better than partial match', () => {
    const results = services.getSuggestions('Montreal')
    const Montreal = results.find(city => city.name.startsWith('Montréal, QC, Canada'))
    const MontrealOuest = results.find(city => city.name.startsWith('Montréal-Ouest, QC, Canada'))
    expect(Montreal).to.exist
    expect(MontrealOuest).to.exist
    expect(Montreal.score).to.be.greaterThan(MontrealOuest.score)
  })
  it('should score 1 for single result', () => {
    const results = services.getSuggestions('Montreal ou')
    expect(results.length).to.equal(1)
    expect(results[0].score).to.equal(1)
  })
  it('should improve score based on latitude & longitude', () => {
    let NewYork
    // New York City location is lat: 40.71427, lon: -74.00597
    let results = services.getSuggestions('New', 0, 0)
    NewYork = results.find(city => city.name.startsWith('New York City'))
    expect(NewYork).to.exist
    const scoreFromAfar = NewYork.score

    results = services.getSuggestions('New', 40, -74)
    NewYork = results.find(city => city.name.startsWith('New York City'))
    expect(NewYork).to.exist
    const scoreFromNear = NewYork.score

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