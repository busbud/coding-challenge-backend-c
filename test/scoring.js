
/**
 * Module dependencies.
 */

var _ = require('underscore');
var expect = require('chai').expect;

var scoring = require('../lib/scoring');
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
    {name: 'a', country: 'Canada', admin1: 'QC', population: 4000, latitude: '45.45008', longitude: '-73.29916'},
    {name: 'b', country: 'Canada', admin1: 'ON', population: 2000, latitude: '42.98339', longitude: '-81.06643'},
    {name: 'c', country: 'USA', admin1: 'CA', population: 6000, latitude: '45.21678', longitude: '-72.51581'},
  ],
};


/**
 * General interface tests.
 */

describe('testing scoring with a caller location', function () {

  var scoredResults;

  before(function (done) {
    var results = sorting.sortResults(
      testData,
      sorting.sortResultsByDistance,
      origin
    );
    scoredResults = scoring.scoreResults(results);
    done();
  });

  it('returns the correct information', function () {
    expect(scoredResults).to.have.length(3);
    expect(scoredResults[0].name).to.equal('b, ON, Canada');
    expect(scoredResults[1].name).to.equal('a, QC, Canada');
    expect(scoredResults[2].name).to.equal('c, CA, USA');
  });
});

describe('testing scoring without a caller location', function () {

  var scoredResults;

  before(function (done) {
    var results = sorting.sortResults(
      testData,
      sorting.sortResultsByPopulation
    );
    scoredResults = scoring.scoreResults(results);
    done();
  });

  it('returns the correct information', function () {
    expect(scoredResults).to.have.length(3);
    expect(scoredResults[0].name).to.equal('c, CA, USA');
    expect(scoredResults[1].name).to.equal('a, QC, Canada');
    expect(scoredResults[2].name).to.equal('b, ON, Canada');
  });
});
