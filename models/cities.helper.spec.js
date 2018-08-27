const {expect}  = require('chai');

const {
  applyDistanceScores,
  applyPopulationScores,
  computeFinalScoreAndFormat,
  getCitiesMatchingPrefix,
  normalizeSearchTerm,
} = require('./cities.helper.js');

describe('models.cities.helper', () => {
  describe('normalizeSearchTerm', () => {
    describe('with a "st-"', () => {
      let result;
      before('call function', () => {
        result = normalizeSearchTerm('st-john');
      });

      it('should have correctly transformed the query', () => {
        expect(result).to.equal('st. john');
      });
    });

    describe('with a "st."', () => {
      let result;
      before('call function', () => {
        result = normalizeSearchTerm('st.john');
      });

      it('should have correctly transformed the query', () => {
        expect(result).to.equal('st. john');
      });
    });

    describe('with a "é"', () => {
      let result;
      before('call function', () => {
        result = normalizeSearchTerm('gaspé');
      });

      it('should have correctly transformed the query', () => {
        expect(result).to.equal('gaspe');
      });
    });

    describe('with an uppercase', () => {
      let result;
      before('call function', () => {
        result = normalizeSearchTerm('Gaspé');
      });

      it('should have correctly transformed the query', () => {
        expect(result).to.equal('gaspe');
      });
    });
  });

  describe('getCitiesMatchingPrefix', () => {
    describe('with correct data', () => {
      let result;
      before('call function', () => {
        const cities = [{
          ascii: 'Montreal',
          'alt_name': [],
        }, {
          ascii: 'Montpellier',
          'alt_name': [],
        }, {
          ascii: 'Denver',
          'alt_name': ['Mont-Denver'],
        }];

        result = getCitiesMatchingPrefix(cities, 'mont');
      });

      it('should return an array of cities with scores', () => {
        expect(result.length).to.equal(3);
        result.forEach(entry => {
          expect(entry.scorePrefix).to.be.a('number');
          expect(entry.scoreString).to.be.a('number');
        });
      });
    });
  });

  describe('applyPopulationScores', () => {
    describe('with correct data', () => {
      let result;
      before('call function', () => {
        const cities = [{
          population: 10,
        }, {
          population: 9,
        }, {
          population: 6,
        }, {
          population: 1,
        }];

        result = applyPopulationScores(cities);
      });

      it('should return an array of cities with scores', () => {
        expect(result.length).to.equal(4);
        result.forEach(entry => {
          expect(entry.scorePopulation).to.be.a('number');
        });
      });

      it('should have applied the correct population scores', () => {
        // In this case the results are all relative to ten, so just divide by ten
        result.forEach(entry => expect(entry.scorePopulation).to.equal(entry.population / 10));
      });
    });
  });

  describe('applyDistanceScores', () => {
    describe('with correct data', () => {
      let result;
      before('call function', () => {
        const cities = [{
          long: -73.19915,
          lat: 45.56678,
        }, {
          long: -72.19915,
          lat: 47.56678,
        }, {
          long: -100.19915,
          lat: 88.56678,
        }];

        result = applyDistanceScores(cities, {longitude: -71.8929, latitude: 45.4042});
      });

      it('should return an array of cities with scores', () => {
        expect(result.length).to.equal(3);
        result.forEach(entry => {
          expect(entry.scoreDistance).to.be.a('number');
        });
      });

      it('should have applied the correct distance scores', () => {
        result.forEach(entry => expect(entry.scoreDistance).to.be.within(0, 1));
      });
    });
  });

  describe('computeFinalScoreAndFormat', () => {
    describe('with correct data', () => {
      let result;
      before('call function', () => {
        const cities = [{
          scorePrefix: 1,
          scoreString: 0.70,
          scoreDistance: 0.18,
          scorePopulation: 0.54,
          distance: 1,
          long: -73.19915,
          lat: 45.56678,
          displayName: 'Some City, Some State, Some Country',
        }, {
          scorePrefix: 1,
          scoreString: 0.15,
          scoreDistance: 0.67,
          scorePopulation: 0.04,
          distance: 0,
          long: -73.19915,
          lat: 45.56678,
          displayName: 'Some City, Some State, Some Country',
        }, {
          scorePrefix: 0.1,
          scoreString: 0.1,
          scoreDistance: 0.9,
          scorePopulation: 0.6,
          distance: 1,
          long: -73.19915,
          lat: 45.56678,
          displayName: 'Some City, Some State, Some Country',
        }];

        result = computeFinalScoreAndFormat(cities);
      });

      it('should return an array of cities with scores', () => {
        expect(result.length).to.equal(3);
        result.forEach(entry => {
          expect(entry.score).to.be.a('number');
        });
      });

      it('should have applied the correct distance scores', () => {
        result.forEach(entry => expect(entry.score).to.be.within(0, 1));
      });

      it('contains names', () => {
        expect(result).to.satisfy(suggestions => suggestions.every(suggestion => !!suggestion.name));
      });

      it('contains latitudes and longitudes', () => {
        expect(result).to.satisfy(suggestions => suggestions.every(suggestion => suggestion.latitude && suggestion.longitude));
      });
    });
  });
});
