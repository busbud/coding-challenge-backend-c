
/**
 * Module dependencies.
 */

var _ = require('underscore');
var expect = require('chai').expect;

var sorting = require('../lib/sorting');


/**
 * Jslint global directives.
 */

/*global before, describe, it */


/**
 * Module variables.
 */

var origin = {latitude: '49.13337', longitude: '-102.98422'};
var testData = {
  0: [
    {name: 'a', population: 4000, latitude: '45.45008', longitude: '-73.29916'},
    {name: 'b', population: 2000, latitude: '42.98339', longitude: '-81.06643'},
    {name: 'c', population: 6000, latitude: '45.21678', longitude: '-72.51581'},
  ],
};


describe('validating sorting functionalities', function () {

/**
 * General interface tests.
 */

  describe('testing sorting on distance', function () {

    var results;

    before(function (done) {
      results = sorting.sortResultsByDistance(testData[0], origin);
      done();
    });

    it('returns the correct information', function () {
      expect(results).to.have.length(3);
      expect(results[0].name).to.equal('b');
      expect(results[1].name).to.equal('a');
      expect(results[2].name).to.equal('c');
    });
  });

  describe('testing sorting on population', function () {

    var results;

    before(function (done) {
      results = sorting.sortResultsByPopulation(testData[0]);
      done();
    });

    it('returns the correct information', function () {
      expect(results).to.have.length(3);
      expect(results[0].name).to.equal('c');
      expect(results[1].name).to.equal('a');
      expect(results[2].name).to.equal('b');
    });
  });

  describe('testing sorting sub-groups on distance', function () {

    var results;

    before(function (done) {
      results = sorting.sortResults(
        testData,
        sorting.sortResultsByDistance,
        origin
      );
      done();
    });

    it('returns the correct information', function () {
      expect(_.keys(results)).to.have.length(1);
      expect(results[0]).to.have.length(3);
      expect(results[0][0].name).to.equal('b');
      expect(results[0][1].name).to.equal('a');
      expect(results[0][2].name).to.equal('c');
    });
  });

  describe('testing sorting sub-groups on population', function () {

    var results;

    before(function (done) {
      results = sorting.sortResults(
        testData,
        sorting.sortResultsByPopulation
      );
      done();
    });

    it('returns the correct information', function () {
      expect(_.keys(results)).to.have.length(1);
      expect(results[0]).to.have.length(3);
      expect(results[0][0].name).to.equal('c');
      expect(results[0][1].name).to.equal('a');
      expect(results[0][2].name).to.equal('b');
    });
  });
});
