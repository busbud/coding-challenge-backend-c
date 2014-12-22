
/**
 * Module dependencies.
 */

var _ = require('underscore');
var expect  = require('chai').expect;
var path = require('path');

var data_store = require('../lib/data_store');


/**
 * Jslint global directives.
 */

/*global describe, before, it */


/**
 * data_store.countryCodeToName.
 */

describe('validating the country/province/state expansion', function () {
  it('looking at the country names', function () {
    expect(data_store.countryCodeToName('BAD')).to.equal('BAD');
    expect(data_store.countryCodeToName('CA')).to.equal('Canada');
    expect(data_store.countryCodeToName('US')).to.equal('USA');
  });
  it('looking at the state/province names', function () {
    expect(data_store.fipsCodeToProvinceCode('BAD')).to.equal('BAD');
    expect(data_store.fipsCodeToProvinceCode('01')).to.equal('AB');
    expect(data_store.fipsCodeToProvinceCode('06')).to.equal('06');
    expect(data_store.fipsCodeToProvinceCode('CA')).to.equal('CA');
    expect(data_store.fipsCodeToProvinceCode('BAD')).to.equal('BAD');
  });
});


/**
 * data_store.setDataSource / data_store.query - string.
 */

describe('setting the data store from a string', function () {
  before(function (done) {
    var CSV_STRING = 'id,ascii,lat,long,country,admin1,population\n' +
                     '1,foo,1,1,US,CA,5500\n' +
                     '2,bar,2,2,CA,10,6000\n' +
                     '3,baz,3,3,CA,01,6500';

    data_store.setDataSource({string: CSV_STRING}, done);
  });

  it('looking up substring of one character with no matches', function () {
    data_store.query('a', function (err, result) {
      expect(err).to.equal(null);
      expect(_.keys(result)).to.have.length(0);
    });
  });
  it('looking up substring of two characters, with one hit', function () {
    data_store.query('fo', function (err, result) {
      expect(err).to.equal(null);
      expect(_.keys(result)).to.have.length(1);
      expect(result['1']).to.have.length(1);
    });
  });
  it('looking up substring of two characters, with two hits', function () {
    data_store.query('ba', function (err, result) {
      expect(err).to.equal(null);
      expect(_.keys(result)).to.have.length(1);
      expect(result['1']).to.have.length(2);
    });
  });
  it('looking up substring of three characters, with one hits', function () {
    data_store.query('bar', function (err, result) {
      expect(err).to.equal(null);
      expect(_.keys(result)).to.have.length(1);
      expect(result['0']).to.have.length(1);
    });
  });
});


/**
 * data_store.setDataSource / data_store.query - file.
 */

describe('setting the data store from a file', function () {
  before(function (done) {
    data_store.setDataSource({
      file: path.resolve(__dirname, '../data/cities_canada-usa.tsv')
    }, done);
  });
  it('looking with Foobar, which yields no results', function () {
    data_store.query('Foobar', function (err, results) {
      expect(err).to.equal(null);
      expect(_.keys(results)).to.have.length(0);
    });
  });
  it('looking with Montreal with uppercase, which yields 2 results', function () {
    data_store.query('Montreal', function (err, results) {
      expect(err).to.equal(null);
      expect(_.keys(results)).to.have.length(2);
      expect(results['0']).to.have.length(1);
      expect(results['6']).to.have.length(1);
    });
  });
  it('looking with Montreal with lowercase, which yields 2 results', function () {
    data_store.query('montreal', function (err, results) {
      expect(err).to.equal(null);
      expect(_.keys(results)).to.have.length(2);
      expect(results['0']).to.have.length(1);
      expect(results['6']).to.have.length(1);
    });
  });
  it('looking with Montréal with accent, which yields 2 results', function () {
    data_store.query('montréal', function (err, results) {
      expect(err).to.equal(null);
      expect(_.keys(results)).to.have.length(2);
      expect(results['0']).to.have.length(1);
      expect(results['6']).to.have.length(1);
    });
  });
  it('testing sorting algo, using city population', function () {
    var queryParam = 'Londo';
    data_store.query(queryParam, function (err, results) {
      expect(err).to.equal(null);
      results = data_store.sortResults(results, data_store.sortResultByPopulation);
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
    data_store.query(queryParam, function (err, results) {
      expect(err).to.equal(null);
      results = data_store.sortResults(results, data_store.sortResultByDistance, queryOrigin);
      expect(_.keys(results)).to.have.length(2);
      expect(results['1']).to.have.length(3);
      expect(results['1'][0].distance).to.be.at.most(results['1'][1].distance);
      expect(results['1'][1].distance).to.be.at.most(results['1'][2].distance);
      expect(results['6']).to.have.length(2);
      expect(results['6'][0].distance).to.be.at.most(results['6'][1].distance);
    });
  });
  it('testing scoring, using population', function () {
    var queryParam = 'Londo';
    data_store.query(queryParam, function (err, results) {
      expect(err).to.equal(null);
      results = data_store.sortResults(results, data_store.sortResultByPopulation);
      expect(_.keys(results)).to.have.length(2);
      var scoredResults = data_store.scoreResult(results);

      expect(scoredResults).to.have.length(5);
      // @FIXME: Must update the scoring results
      expect(scoredResults[0].name).to.equal('London, ON, Canada');
      expect(scoredResults[0].score).to.equal(0.8);
      expect(scoredResults[1].name).to.equal('London, OH, USA');
      expect(scoredResults[1].score).to.equal(0.8);
      expect(scoredResults[2].name).to.equal('London, KY, USA');
      expect(scoredResults[2].score).to.equal(0.8);
      expect(scoredResults[3].name).to.equal('Londonderry, NH, USA');
      expect(scoredResults[3].score).to.equal(0.8);
      expect(scoredResults[4].name).to.equal('Londontowne, MD, USA');
      expect(scoredResults[4].score).to.equal(0.8);
    });
  });
  it('testing scoring, using caller location', function () {
    var queryParam = 'Londo';
    var queryOrigin = {latitude: '43.70011', longitude: '-79.4163'};
    data_store.query(queryParam, function (err, results) {
      expect(err).to.equal(null);
      results = data_store.sortResults(results, data_store.sortResultByDistance, queryOrigin);
      expect(_.keys(results)).to.have.length(2);
      var scoredResults = data_store.scoreResult(results);

      expect(scoredResults).to.have.length(5);
      // @FIXME: Must update the scoring results
      expect(scoredResults[0].name).to.equal('London, ON, Canada');
      expect(scoredResults[0].score).to.equal(0.8);
      expect(scoredResults[1].name).to.equal('London, OH, USA');
      expect(scoredResults[1].score).to.equal(0.8);
      expect(scoredResults[2].name).to.equal('London, KY, USA');
      expect(scoredResults[2].score).to.equal(0.8);
      expect(scoredResults[3].name).to.equal('Londontowne, MD, USA');
      expect(scoredResults[3].score).to.equal(0.8);
      expect(scoredResults[4].name).to.equal('Londonderry, NH, USA');
      expect(scoredResults[4].score).to.equal(0.8);
    });
  });
});
