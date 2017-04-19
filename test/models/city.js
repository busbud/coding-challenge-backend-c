const assert = require('chai').assert;

const getDb = require('../../src/database/reader').getDb;
const City = require('../../src/models/city').default;


describe('City model', function() {

  describe('for the country of Canada', function () {
    let city;

    before(function (done) {
      getDb().then(() => {
        city = City.create({
          name: 'Montréal',
          asciiName: 'Montreal',
          latitude: 40,
          longitude: 80,
          country: 'CA',
          admin1: '10',
          population: 2000000,
          searchableName: "MONTREAL",
        });
        city.geoScore = 0.5;
        city.nameScore = 0.5;
        done();
      });
    });

    it('should provide the correct Province label', function() {
      assert.equal(city.stateLabel, 'QC');
    });

    it('should provide the correct Country label', function() {
      assert.equal(city.countryLabel, 'Canada');
    });

    it('should provide the correct display label', function() {
      assert.equal(city.displayLabel, 'Montreal, QC, Canada');
    });

    it('should provide a suitable API Response Object', function() {
      const expected = {
        name: 'Montreal, QC, Canada',
        latitude: '40',
        longitude: '80',
        score: city.totalScore
      };
      const actual = city.toApiResponseObject();
      const actualKeys = Object.keys(actual);
      assert.equal(actual.name, expected.name);
      assert.equal(actual.latitude, expected.latitude);
      assert.equal(actual.longitude, expected.longitude);
      assert.equal(actual.score, expected.score);
      assert.ok(actualKeys.indexOf('name') > -1);
      assert.ok(actualKeys.indexOf('latitude') > -1);
      assert.ok(actualKeys.indexOf('longitude') > -1);
      assert.ok(actualKeys.indexOf('score') > -1);
    });
  });

  describe('for the country of Canada having a bad province', function () {
    let city;

    before(function (done) {
      getDb().then(() => {
        city = City.create({
          name: 'Nope',
          asciiName: 'Nope',
          latitude: 40,
          longitude: 80,
          country: 'CA',
          admin1: '99999',
          population: 2000000,
          searchableName: "NOPE",
        });
        city.geoScore = 0.5;
        city.nameScore = 0.5;
        done();
      });
    });

    it('should provide an empty Province label', function() {
      assert.equal(city.stateLabel, '');
    });

    it('should provide the correct Country label', function() {
      assert.equal(city.countryLabel, 'Canada');
    });

    it('should provide the correct display label', function() {
      assert.equal(city.displayLabel, 'Nope, Canada');
    });

  });

  describe('for the country of USA', function () {
    let city;

    before(function (done) {
      getDb().then(() => {
        city = City.create({
          name: 'Cleveland',
          asciiName: 'Cleveland',
          latitude: 40,
          longitude: 80,
          country: 'US',
          admin1: 'OH',
          population: 2000000,
          searchableName: "CLEVELAND",
        });
        city.geoScore = 0.5;
        city.nameScore = 0.5;
        done();
      });
    });

    it('should provide the correct State label', function() {
      assert.equal(city.stateLabel, 'OH');
    });

    it('should provide the correct Country label', function() {
      assert.equal(city.countryLabel, 'United States of America');
    });

    it('should provide the correct display label', function() {
      assert.equal(city.displayLabel, 'Cleveland, OH, United States of America');
    });

  });

  describe('for bad country data', function () {
    let city;

    before(function (done) {
      getDb().then(() => {
        city = City.create({
          name: 'Question Mark',
          asciiName: 'Question Mark',
          latitude: 40,
          longitude: 80,
          country: 'dsfnsdkhgbdsag',
          admin1: 'sdbnvakbgdsgbf',
          population: 2000000,
          searchableName: "QUESTION MARK",
        });
        city.geoScore = 0.5;
        city.nameScore = 0.5;
        done();
      });
    });

    it('should provide an empty State label', function() {
      assert.equal(city.stateLabel, '');
    });

    it('should provide an empty Country label', function() {
      assert.equal(city.countryLabel, '');
    });

    it('should provide the correct display label', function() {
      assert.equal(city.displayLabel, 'Question Mark');
    });

  });

  it('should provide the name score if there is no geo score', function(done) {
    getDb().then(() => {
      const city = City.create({});
      city.nameScore = 0.2;
      assert.equal(city.totalScore, 0.2);
      done();
    });

  });

  it('should provide the geo score if there is no name score', function(done) {
    getDb().then(() => {
      const city = City.create({});
      city.geoScore = 0.1;
      assert.equal(city.totalScore, 0.1);
      done();
    });
  });

  it('should provide its collection name', function(done) {
    assert.equal(City.collectionName(), 'cities');
    done();
  });

  it('should provide its collection file path', function(done) {
    /cities.db$/.test(City.filePath());
    done();
  });

});
