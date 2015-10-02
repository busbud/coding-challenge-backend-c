var haversine = require('../lib/haversine');
var expect = require('chai').expect;

describe('haversine', function() {
  describe('random long lat', function() {
    it('is ~0.356', function () {
      expect(haversine(43.1, -50.3, -2.13, -81.05)).to.be.within(0.356, 0.357);
    });
  });

  describe('same long/lat = 1', function () {
    it('test', function() {
      expect(haversine(43.0, 43.0, -70.0, -70.0)).to.equal(1);
    })
  })

  describe('along equator = 0', function () {
    it('test', function() {
      expect(haversine(0.0, 0.0, 180.0, 0.0)).to.equal(0);
    })
  })
})