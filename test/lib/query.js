var expect  = require('chai').expect;
var _  = require('lodash');

var Query = require('../../lib/query');
var GeoPoint = require('../../lib/geo-point');
var InvalidParameterError = require('../../lib/errors').InvalidParameterError;

describe('Query', function () {
  describe('#constructor', function () {
    it('should throw an error when no q params is passed', function () {
      expect(function createInvalidQuery() {
        var query = new Query({}); 
      }).to.throw(InvalidParameterError);
    });

    it('should throw an error when  q params is not a string', function () {
      expect(function createInvalidQuery() {
        var query = new Query({q: []}); 
      }).to.throw(InvalidParameterError);
    });

    it('should throw an error when  q params is not a string', function () {
      expect(function createInvalidQuery() {
        var query = new Query({q: []}); 
      }).to.throw(InvalidParameterError);
    });
    
    it('should return a query with a geoPoint when lat and lng are passed', function () {
      var query = new Query({q: 'a query', latitude: -10, longitude: 10.0});
      expect(query).to.have.all.keys('q', 'geoPoint');
    });

    it('should return a query instance that does not contain a geoPoint when lat and lng are not passed', function () {
      var query = new Query({q: 'a query'});
      expect(query).to.have.all.keys('q');
    });
    
  });
});