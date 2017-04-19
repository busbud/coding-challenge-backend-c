const rewire = require('rewire');
const assert = require('chai').assert;
const City = require('../../src/models/city').default;
const scoreCitiesByRelevancy = require('../../src/suggestions/scoring').default;

const scoring = rewire('../../src/suggestions/scoring.js');

// Access and test private functions
const earthDiameter = scoring.__get__('earthDiameter');
const computeGeoScore = scoring.__get__('computeGeoScore');
const computeNameScore = scoring.__get__('computeNameScore');

describe('Scoring module', function() {

  describe('city geo scoring', function () {
    it('should compute a score of 1 for a perfect match', function(done) {
      const geoScore = computeGeoScore(0);
      assert.equal(geoScore, 1);
      done();
    });

    it('should compute a score between 0 and 1 for any distance between extremes', function(done) {
      const geoScore = computeGeoScore(12345);
      assert.ok(geoScore > 0);
      assert.ok(geoScore < 1);
      done();
    });

    it('should compute a score of 0 for the maximum possible distance', function(done) {
      const geoScore = computeGeoScore(earthDiameter);
      assert.equal(geoScore, 0);
      done();
    });


  });

  describe('city name scoring', function () {
    it('should compute a score of 1 for a perfect match', function(done) {
      const nameScore = computeNameScore('Test', 'Test');
      assert.equal(nameScore, 1);
      done();
    });

    it('should compute a score of 0.5 for a half-match', function(done) {
      const nameScore = computeNameScore('Te', 'Test');
      assert.equal(nameScore, 0.5);
      done();
    });

    it('should compute return null for an empty query', function(done) {
      const nameScore = computeNameScore('', 'Test');
      assert.equal(nameScore, null);
      done();
    });
  });

  describe('obtaining scored results from name scoring', function () {
    let suggestions;
    let cityArray;

    before(function (done) {
      cityArray = [];
      cityArray.push(City.create({name: 'Fake-City', latitude: 40, longitude: 80}));
      cityArray.push(City.create({name: 'Fake-Mountain', latitude: 42.5, longitude: 82.5}));
      cityArray.push(City.create({name: 'Faker-Suburb', latitude: 45, longitude: 85}));
      cityArray.push(City.create({name: 'Fakerstown', latitude: 50, longitude: 90}));
      cityArray.push(City.create({name: 'Fake-Village', latitude: 60, longitude: 100}));
      cityArray.push(City.create({name: 'Random Place', latitude: 22, longitude: 121}));

      scoreCitiesByRelevancy(cityArray, 'Fake', null, null, 3).then(res => {
        suggestions = res;
        done();
        }).catch(err => {
          done(err);
      });
    });


    it('should contain at most 3 results out of the possible 5', function() {
      assert.equal(suggestions.length, 3);
    });

    it('should contain results sorted by total score', function() {
      assert.ok(suggestions[0].totalScore >= suggestions[1].totalScore);
      assert.ok(suggestions[1].totalScore >= suggestions[2].totalScore);
    });

    it('should have Fake-City as the best result', function() {
      assert.equal(suggestions[0].name, 'Fake-City');
    });

  });

  describe('obtaining scored results from geo and name scoring', function () {
    let suggestions;
    let cityArray;

    before(function (done) {
      cityArray = [];
      cityArray.push(City.create({name: 'Fake-City', latitude: 10, longitude: 10}));
      cityArray.push(City.create({name: 'Fake-Mountain', latitude: 42.5, longitude: 82.5}));
      cityArray.push(City.create({name: 'Faker-Suburb', latitude: 41, longitude: 88}));
      cityArray.push(City.create({name: 'Fakerstown', latitude: 50, longitude: 90}));
      cityArray.push(City.create({name: 'Fake-Village', latitude: 790, longitude: 109}));
      cityArray.push(City.create({name: 'Random Place', latitude: 22, longitude: 121}));

      scoreCitiesByRelevancy(cityArray, 'Fake', 50, 90, 3).then(res => {
        suggestions = res;
        done();
      }).catch(err => {
        done(err);
      });
    });


    it('should contain at most 3 results out of the possible 5', function() {
      assert.equal(suggestions.length, 3);
    });

    it('should contain results sorted by total score', function() {
      assert.ok(suggestions[0].totalScore >= suggestions[1].totalScore);
      assert.ok(suggestions[1].totalScore >= suggestions[2].totalScore);
    });

    it('should have Fakerstown as the best result', function() {
      assert.equal(suggestions[0].name, 'Fakerstown');
    });

  });

});
