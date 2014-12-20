
/**
 * Module dependencies.
 */

var expect  = require('chai').expect;
var data_store = require('../lib/data_store');
var path = require('path');


/**
 * Jslint global directives.
 */

/*global describe, before, it */


/**
 * data_store.countryCodeToName.
 */

describe('Validating the country/province/state expansion', function () {
  it('Looking at the country names', function () {
    expect(data_store.countryCodeToName('BAD')).to.equal('BAD');
    expect(data_store.countryCodeToName('CA')).to.equal('Canada');
    expect(data_store.countryCodeToName('US')).to.equal('USA');
  });
  it('Looking at the state/province names', function () {
    expect(data_store.fipsCodeToName('BADBAD')).to.equal('BADBAD');
    expect(data_store.fipsCodeToName('CA01')).to.equal('Alberta');
    expect(data_store.fipsCodeToName('CABAD')).to.equal('CABAD');
    expect(data_store.fipsCodeToName('USCA')).to.equal('California');
    expect(data_store.fipsCodeToName('USBAD')).to.equal('USBAD');
  });
});


/**
 * data_store.setDataSource / data_store.query - string.
 */

describe('Setting the data store from a string', function () {
  before(function (done) {
    var CSV_STRING = 'id,ascii,lat,long,country,admin1,population\n' +
                     '1,foo,1,1,US,CA,5500\n' +
                     '2,bar,2,2,CA,10,6000\n' +
                     '3,baz,3,3,CA,01,6500';

    data_store.setDataSource({string: CSV_STRING}, done);
  });

  it('Looking up substring of one character with no matches', function () {
    data_store.query('a', function (err, result) {
      expect(err).to.equal(null);
      expect(result).to.have.length(0);
    });
  });
  it('Looking up substring of two characters, with one hit', function () {
    data_store.query('fo', function (err, result) {
      expect(err).to.equal(null);
      expect(result).to.have.length(1);
    });
  });
  it('Looking up substring of two characters, with two hits', function () {
    data_store.query('ba', function (err, result) {
      expect(err).to.equal(null);
      expect(result).to.have.length(2);
    });
  });
  it('Looking up substring of three characters, with one hits', function () {
    data_store.query('bar', function (err, result) {
      expect(err).to.equal(null);
      expect(result).to.have.length(1);
    });
  });

});


/**
 * data_store.setDataSource / data_store.query - file.
 */

describe('Setting the data store from a file', function () {
  before(function (done) {
    data_store.setDataSource({file: path.resolve(__dirname, '../data/cities_canada-usa.tsv')}, done);
  });

  it('Looking with Foobar, which yields no results', function () {
    data_store.query('Foobar', function (err, result) {
      expect(err).to.equal(null);
      expect(result).to.have.length(0);
    });
  });
  it('Looking with Montreal with uppercase, which yields 2 results', function () {
    data_store.query('Montreal', function (err, result) {
      expect(err).to.equal(null);
      expect(result).to.have.length(2);
    });
  });
  it('Looking with Montreal with lowercase, which yields 2 results', function () {
    data_store.query('montreal', function (err, result) {
      expect(err).to.equal(null);
      expect(result).to.have.length(2);
    });
  });
  it('Looking with Montréal with accent, which yields 2 results', function () {
    data_store.query('montréal', function (err, result) {
      expect(err).to.equal(null);
      expect(result).to.have.length(2);
    });
  });

});
