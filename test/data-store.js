
/**
 * Module dependencies.
 */

var _ = require('underscore');
var expect  = require('chai').expect;
var path = require('path');

var dataStore = require('../lib/data-store');
var scoring = require('../lib/scoring');
var sorting = require('../lib/sorting');


/**
 * Jslint global directives.
 */

/*global describe, before, it */


describe('validating data-store functionalities', function () {

/**
 * dataStore.countryCodeToName.
 */

  describe('validating the country/province/state expansion', function () {
    it('looking at the country names', function () {
      expect(dataStore.countryCodeToName('BAD')).to.equal('BAD');
      expect(dataStore.countryCodeToName('CA')).to.equal('Canada');
      expect(dataStore.countryCodeToName('US')).to.equal('USA');
    });
    it('looking at the state/province names', function () {
      expect(dataStore.fipsCodeToProvinceCode('BAD')).to.equal('BAD');
      expect(dataStore.fipsCodeToProvinceCode('01')).to.equal('AB');
      expect(dataStore.fipsCodeToProvinceCode('06')).to.equal('06');
      expect(dataStore.fipsCodeToProvinceCode('CA')).to.equal('CA');
      expect(dataStore.fipsCodeToProvinceCode('BAD')).to.equal('BAD');
    });
  });


/**
 * dataStore.setDataSource / dataStore.query - string.
 */

  describe('setting the data store from a string', function () {
    before(function (done) {
      var CSV_STRING = 'id,ascii,lat,long,country,admin1,population\n' +
                       '1,foo,1,1,US,CA,5500\n' +
                       '2,bar,2,2,CA,10,6000\n' +
                       '3,baz,3,3,CA,01,6500';

      dataStore.setDataSource({string: CSV_STRING}, done);
    });

    it('looking up substring of one character with no matches', function () {
      dataStore.query('a', function (err, result) {
        expect(err).to.equal(null);
        expect(_.keys(result)).to.have.length(0);
      });
    });
    it('looking up substring of two characters, with one hit', function () {
      dataStore.query('fo', function (err, result) {
        expect(err).to.equal(null);
        expect(_.keys(result)).to.have.length(1);
        expect(result['1']).to.have.length(1);
      });
    });
    it('looking up substring of two characters, with two hits', function () {
      dataStore.query('ba', function (err, result) {
        expect(err).to.equal(null);
        expect(_.keys(result)).to.have.length(1);
        expect(result['1']).to.have.length(2);
      });
    });
    it('looking up substring of three characters, with one hits', function () {
      dataStore.query('bar', function (err, result) {
        expect(err).to.equal(null);
        expect(_.keys(result)).to.have.length(1);
        expect(result['0']).to.have.length(1);
      });
    });
  });


/**
 * dataStore.setDataSource / dataStore.query - file.
 */

  describe('setting the data store from a file', function () {
    before(function (done) {
      dataStore.setDataSource({
        file: path.resolve(__dirname, '../data/cities_canada-usa.tsv')
      }, done);
    });
    it('looking with Foobar, which yields no results', function () {
      dataStore.query('Foobar', function (err, results) {
        expect(err).to.equal(null);
        expect(_.keys(results)).to.have.length(0);
      });
    });
    it('looking with Montreal with uppercase, which yields 2 results', function () {
      dataStore.query('Montreal', function (err, results) {
        expect(err).to.equal(null);
        expect(_.keys(results)).to.have.length(2);
        expect(results['0']).to.have.length(1);
        expect(results['6']).to.have.length(1);
      });
    });
    it('looking with Montreal with lowercase, which yields 2 results', function () {
      dataStore.query('montreal', function (err, results) {
        expect(err).to.equal(null);
        expect(_.keys(results)).to.have.length(2);
        expect(results['0']).to.have.length(1);
        expect(results['6']).to.have.length(1);
      });
    });
    it('looking with Montréal with accent, which yields 2 results', function () {
      dataStore.query('montréal', function (err, results) {
        expect(err).to.equal(null);
        expect(_.keys(results)).to.have.length(2);
        expect(results['0']).to.have.length(1);
        expect(results['6']).to.have.length(1);
      });
    });
    it('testing sorting algo, using city population', function () {
      var queryParam = 'Londo';
      dataStore.query(queryParam, function (err, results) {
        expect(err).to.equal(null);
        results = sorting.sortResults(results, sorting.sortResultsByPopulation);
        expect(_.keys(results)).to.have.length(2);
        expect(results['1']).to.have.length(3);
        expect(results['1'][0].population).to.be.at.least(results['1'][1].population);
        expect(results['1'][1].population).to.be.at.least(results['1'][2].population);
        expect(results['6']).to.have.length(2);
        expect(results['6'][0].population).to.be.at.least(results['6'][1].population);
      });
    });
    it('testing sorting algo, using caller location', function () {
      var queryParam = 'Londo';
      var queryOrigin = {latitude: '43.70011', longitude: '-79.4163'};
      dataStore.query(queryParam, function (err, results) {
        expect(err).to.equal(null);
        results = sorting.sortResults(results, sorting.sortResultsByDistance, queryOrigin);
        expect(_.keys(results)).to.have.length(2);
        expect(results['1']).to.have.length(3);
        expect(results['1'][0].distance).to.be.at.most(results['1'][1].distance);
        expect(results['1'][1].distance).to.be.at.most(results['1'][2].distance);
        expect(results['6']).to.have.length(2);
        expect(results['6'][0].distance).to.be.at.most(results['6'][1].distance);
      });
    });
    it('testing scoring, using population', function () {
      var queryParam = 'Ken';
      dataStore.query(queryParam, function (err, results) {
        expect(err).to.equal(null);
        results = sorting.sortResults(results, sorting.sortResultsByPopulation);

        expect(_.keys(results)).to.have.length(10);
        var scoredResults = scoring.scoreResults(results);

        expect(scoredResults).to.have.length(10);
        expect(scoredResults[0].name).to.equal('Kent, WA, USA');
        expect(scoredResults[0].score).to.equal(0.9);
        expect(scoredResults[1].name).to.equal('Kent, OH, USA');
        expect(scoredResults[1].score).to.equal(0.5);
        expect(scoredResults[2].name).to.equal('Kenai, AK, USA');
        expect(scoredResults[2].score).to.equal(0.5);
        expect(scoredResults[3].name).to.equal('Kenner, LA, USA');
        expect(scoredResults[3].score).to.equal(0.2);
        expect(scoredResults[4].name).to.equal('Kenton, OH, USA');
        expect(scoredResults[4].score).to.equal(0.2);
        expect(scoredResults[5].name).to.equal('Kenosha, WI, USA');
        expect(scoredResults[5].score).to.equal(0.1);
        expect(scoredResults[6].name).to.equal('Kendall, FL, USA');
        expect(scoredResults[6].score).to.equal(0.1);
        expect(scoredResults[7].name).to.equal('Kenmore, WA, USA');
        expect(scoredResults[7].score).to.equal(0.1);
        expect(scoredResults[8].name).to.equal('Kenmore, NY, USA');
        expect(scoredResults[8].score).to.equal(0.1);
        expect(scoredResults[9].name).to.equal('Kennett, MO, USA');
        expect(scoredResults[9].score).to.equal(0.1);
      });
    });
    it('testing scoring, using caller location', function () {
      var queryParam = 'Londo';
      var queryOrigin = {latitude: '43.70011', longitude: '-79.4163'};
      dataStore.query(queryParam, function (err, results) {
        expect(err).to.equal(null);
        results = sorting.sortResults(results, sorting.sortResultsByDistance, queryOrigin);
        expect(_.keys(results)).to.have.length(2);
        var scoredResults = scoring.scoreResults(results);

        expect(scoredResults).to.have.length(5);

        expect(scoredResults[0].name).to.equal('London, ON, Canada');
        expect(scoredResults[0].score).to.equal(0.9);
        expect(scoredResults[1].name).to.equal('London, OH, USA');
        expect(scoredResults[1].score).to.equal(0.5);
        expect(scoredResults[2].name).to.equal('London, KY, USA');
        expect(scoredResults[2].score).to.equal(0.5);
        expect(scoredResults[3].name).to.equal('Londontowne, MD, USA');
        expect(scoredResults[3].score).to.equal(0.3);
        expect(scoredResults[4].name).to.equal('Londonderry, NH, USA');
        expect(scoredResults[4].score).to.equal(0.3);
      });
    });
  });
});
