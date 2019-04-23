const chai = require('chai');
const expect = chai.expect;
chai.should();
chai.use(require('chai-things'));
const dataUtils = require('../data-utils');

describe('data-utils', () => {
  const testCitiesData = [
    { population: 10000, country: 'US' },
    { population: 7500, country: 'US' },
    { population: 4000, country: 'US' },
    { population: 100, country: 'CA' },
    { population: 20000, country: 'CA' },
    { population: 5500, country: 'CA' },
    { population: 5500, country: 'XX' }
  ];

  describe('filterDataByPopulation', () => {
    it('should filter to 5000 with no argument', done => {
      dataUtils.filterDataByPopulation(testCitiesData)
        .then(filteredCityData => {
          expect(filteredCityData).to.have.length.at.least(1);
          filteredCityData.forEach((cityData, i) => expect(cityData.population).to.be.at.least(5000, `Filtered city data ${i} was not at least 5000`));
        })
        .then(() => done())
        .catch(err => done(err))
      ;
    });

    it('should filter to argument when provided', done => {
      dataUtils.filterDataByPopulation(testCitiesData, 9000)
        .then(filteredCityData => {
          expect(filteredCityData).to.have.length.at.least(1);
          filteredCityData.forEach((cityData, i) => expect(cityData.population).to.be.at.least(9000, `Filtered city data ${i} was not at least 9000`));
        })
        .then(() => done())
        .catch(err => done(err))
      ;
    });
  });

  describe('filterDataByCountry', () => {
    it('should filter to US & CA with no argument', done => {
      dataUtils.filterDataByCountry(testCitiesData)
        .then(filteredCityData => {
          expect(filteredCityData).to.have.length.at.least(1);
          filteredCityData.forEach((cityData, i) => expect(cityData.country).to.be.oneOf(['US', 'CA'], `Filtered city data ${i} was one of US, CA`));
        })
        .then(() => done())
        .catch(err => done(err))
      ;
    });

    it('should filter to argument when provided', done => {
      dataUtils.filterDataByCountry(testCitiesData, ['XX'])
        .then(filteredCityData => {
          expect(filteredCityData).to.have.length.at.least(1);
          filteredCityData.forEach((cityData, i) => expect(cityData.country).to.equal('XX', `Filtered city data ${i} was not correctly filtered`));
        })
        .then(() => done())
        .catch(err => done(err))
      ;
    });
  });

  describe('sortDataByPopulationDesc', () => {
    it('should sort in descending order', done => {
      dataUtils.sortDataByPopulationDesc(testCitiesData)
        .then(filteredCityData => {
          expect(filteredCityData).to.have.length.at.least(1);
          filteredCityData.forEach((cityData, i) => {
            const lastElementPopulation = i > 0 ? filteredCityData[i - 1].population : Number.MAX_VALUE;
            expect(cityData.population).to.be.at.most(lastElementPopulation, `Filtered city data ${i}'s population was not at most ${i - 1}'s`);
          });
        })
        .then(() => done())
        .catch(err => done(err))
      ;
    });
  });
});
