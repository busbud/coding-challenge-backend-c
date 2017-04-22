const rewire = require('rewire');
const assert = require('chai').assert;
const City = require('../../src/models/city').default;
const getDb = require('../../src/database/reader').getDb;

const api = rewire('../../src/suggestions/api.js');

// Access and test private functions
const buildApiResponse = api.__get__('buildApiResponse');

describe('Suggestions API module', function() {

  describe('building API responses', function () {
    it('should convert arrays of City objects to the desired format', function(done) {

      getDb().then(() => {
        const cities = [];
        const montreal = City.create({
          name: 'Montréal',
          asciiName: 'Montreal',
          latitude: 40,
          longitude: 80,
          country: 'CA',
          admin1: '10',
          population: 2000000,
          searchableName: "MONTREAL",
        });
        montreal.geoScore = 0.5;
        montreal.nameScore = 0.5;
        cities.push(montreal);

        const cleveland = City.create({
          name: 'Cleveland',
          asciiName: 'Cleveland',
          latitude: 40,
          longitude: 80,
          country: 'US',
          admin1: 'OH',
          population: 2000000,
          searchableName: "CLEVELAND",
        });
        cleveland.geoScore = 0.5;
        cleveland.nameScore = 0.5;
        cities.push(cleveland);

        const apiResponseCities = buildApiResponse(cities);

        assert.equal(apiResponseCities.length, 2);

        const expected = {
          name: 'Montreal, QC, Canada',
          latitude: '40',
          longitude: '80',
          score: montreal.totalScore
        };
        const actual = apiResponseCities[0];
        const actualKeys = Object.keys(actual);
        assert.equal(actual.name, expected.name);
        assert.equal(actual.latitude, expected.latitude);
        assert.equal(actual.longitude, expected.longitude);
        assert.equal(actual.score, expected.score);
        assert.ok(actualKeys.indexOf('name') > -1);
        assert.ok(actualKeys.indexOf('latitude') > -1);
        assert.ok(actualKeys.indexOf('longitude') > -1);
        assert.ok(actualKeys.indexOf('score') > -1);

        done();
      });
    });

  });

});
