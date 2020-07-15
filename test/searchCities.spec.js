const expect = require('chai').expect;
const searchCities = require('../src/searchCities');

describe('search cities ', function() {
  it('with a non-existent city returns not hit', async function() {
    const result = await searchCities.search('SomeRandomCity');
    expect(result.hits.total.value).to.equal(0);
  });

  it('with a with a valid city', async function() {
    const result = await searchCities.search('montreal');
    expect(result.hits.total.value).not.equal(0);
  });

  it('with a with a valid city, latitude and longitude', async function() {
    const result = await searchCities.search('lond', 43.70011, 79.4163);
    expect(result.hits.total.value).not.equal(0);
  });
});
