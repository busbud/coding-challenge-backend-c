var format = require('util').format;
var expect  = require('chai').expect;

var GeoPoint = require('../../lib/geo-point');
var InvalidParameterError = require('../../lib/errors').InvalidParameterError;


describe('#GeoPoint constructor', function () {
  describe('with valid params', function () {
    it('should return a GeoPoint when params are valid strings', function () {
      var point = new GeoPoint('89.99999', '-179.9999');
      expect(point).to.be.an.instanceof(GeoPoint)
      expect(point).to.have.property('lat', 89.99999);
      expect(point).to.have.property('lng', -179.9999);
    });
    it('should return a GeoPoint when params are valid float', function () {
      var point = new GeoPoint(10.0, -179.9999);
      expect(point).to.be.an.instanceof(GeoPoint);
      expect(point).to.have.property('lat', 10.0);
      expect(point).to.have.property('lng', -179.9999);
    });
    it('should return a GeoPoint when params are valid integer', function () {
      var point = new GeoPoint(-89, 0);
      expect(point).to.be.an.instanceof(GeoPoint);
      expect(point).to.have.property('lat', -89);
      expect(point).to.have.property('lng', 0);
    });
  });

  describe('with invalid params', function () {
    var tests = [{
      desc: 'no parameters are passed',
      vals: [],
      errorPattern: / latitude /i,
    }, {
      desc: 'one parameter is null',
      vals: [null, 0],
      errorPattern: / latitude /i,
    }, {
      desc: 'one parameter is a string',
      vals: [0, '10.9 followed by a string'],
      errorPattern: / longitude /i,
    }, {
      desc: 'one parameter is an object',
      vals: [0, {a: 0, b: 180}],
      errorPattern: / longitude /i,
    }, {
      desc: 'one parameter is out of bound',
      vals: [0, 181],
      errorPattern: / longitude /i,
    }, {
      desc: 'one parameter is out of bound',
      vals: [-91, 0],
      errorPattern: / latitude /i,
    }];
    tests.forEach(function (test) {
      describe(format('when %s (%s)', test.desc,JSON.stringify(test.vals)), function () {
        it('should throw an exception', function () {
          expect(function createPoint() {
            var point = new GeoPoint(test.vals[0], test.vals[1]);
            console.log('POINT', point);
          }).to.throw(InvalidParameterError, test.errorPattern);
        });
      });
    });
  });
  
    describe('#getDistance', function () {
      it('should return 0 if the points have the same coordinates', function () {
        var point1 = new GeoPoint(10, -10);
        var point2 = new GeoPoint(10, -10);

        expect(point1.getDistance(point2)).to.eql(0);
      });
      it('should return a positive number if the points do have the same coordinates', function () {
        var point1 = new GeoPoint(10, -10);
        var point2 = new GeoPoint(10, -11);

        expect(point1.getDistance(point2)).to.be.gt(-109000);
      });
    });

});
