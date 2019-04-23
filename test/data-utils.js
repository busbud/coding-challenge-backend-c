const chai = require('chai');
const expect = chai.expect;
chai.should();
chai.use(require('chai-things'));
const data_utils = require('../data-utils');

describe('data-utils', () => {
  const test_cities_data = [
    { population: 10000, country: 'US' },
    { population: 7500, country: 'US' },
    { population: 4000, country: 'US' },
    { population: 100, country: 'CA' },
    { population: 20000, country: 'CA' },
    { population: 5500, country: 'CA' },
    { population: 5500, country: 'XX' }
  ];

  describe('filterDataByMinPopulation', () => {
    it('should filter to 5000 with no argument', done => {
      data_utils.filterDataByMinPopulation(test_cities_data)
        .then(filtered_city_data => {
          expect(filtered_city_data).to.have.length.at.least(1);
          filtered_city_data.forEach((c, i) => expect(c.population).to.be.at.least(5000, `Filtered city data ${i} was not at least 5000`));
        })
        .then(() => done())
        .catch(err => done(err))
      ;
    });

    it('should filter to argument when provided', done => {
      data_utils.filterDataByMinPopulation(test_cities_data, 9000)
        .then(filtered_city_data => {
          expect(filtered_city_data).to.have.length.at.least(1);
          filtered_city_data.forEach((c, i) => expect(c.population).to.be.at.least(9000, `Filtered city data ${i} was not at least 9000`));
        })
        .then(() => done())
        .catch(err => done(err))
      ;
    });
  });

  describe('filterDataByCountry', () => {
    it('should filter to US & CA with no argument', done => {
      data_utils.filterDataByCountry(test_cities_data)
        .then(filtered_city_data => {
          expect(filtered_city_data).to.have.length.at.least(1);
          filtered_city_data.forEach((c, i) => expect(c.country).to.be.oneOf(['US', 'CA'], `Filtered city data ${i} was one of US, CA`));
        })
        .then(() => done())
        .catch(err => done(err))
      ;
    });

    it('should filter to argument when provided', done => {
      data_utils.filterDataByCountry(test_cities_data, ['XX'])
        .then(filtered_city_data => {
          expect(filtered_city_data).to.have.length.at.least(1);
          filtered_city_data.forEach((c, i) => expect(c.country).to.equal('XX', `Filtered city data ${i} was not correctly filtered`));
        })
        .then(() => done())
        .catch(err => done(err))
      ;
    });
  });

  describe('sortDataByPopulationDesc', () => {
    it('should sort in descending order', done => {
      data_utils.sortDataByPopulationDesc(test_cities_data)
        .then(filtered_city_data => {
          expect(filtered_city_data).to.have.length.at.least(1);
          filtered_city_data.forEach((c, i) => {
            const lastElementPopulation = i > 0 ? filtered_city_data[i - 1].population : Number.MAX_VALUE;
            expect(c.population).to.be.at.most(lastElementPopulation, `Filtered city data ${i}'s population was not at most ${i - 1}'s`);
          });
        })
        .then(() => done())
        .catch(err => done(err))
      ;
    });
  });
});
