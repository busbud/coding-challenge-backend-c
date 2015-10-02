var levenshtein = require('../levenshtein');
var expect = require('chai').expect;

describe('levenshtein', function() {
  describe('kitten sitting', function() {
    it('is 3', function () {
      expect(levenshtein('kitten', 'sitting')).to.equal(3)
    });

    it('is 3 with params reversed', function() {
      expect(levenshtein('sitting', 'kitten')).to.equal(3)
    });
  });

  describe('jack jane', function() {
    it('is 2', function () {
      expect(levenshtein('jackson', 'jane')).to.equal(5)
    });
  });

  describe('empty cases', function() {
    it('both empty', function () {
      expect(levenshtein('', '')).to.equal(0)
    });

    it('first empty', function() {
      expect(levenshtein('', 'kitten')).to.equal(6)
    });

    it('last empty', function() {
      expect(levenshtein('sitting', '')).to.equal(7)
    });
  });

  describe('same string', function() {
    it('test', function () {
      expect(levenshtein('test', 'test')).to.equal(0)
    });
  });
})