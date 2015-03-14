var format = require('util').format;
var expect  = require('chai').expect;
var _  = require('lodash');

var Location = require('../../lib/location');
var GeoPoint = require('../../lib/geo-point');
var Query = require('../../lib/query');
var InvalidParameterError = require('../../lib/errors').InvalidParameterError;

var MtlData = {name: 'Montreal', latitude: 45.50884, longitude: -73.58781};

describe('Location class', function () {
  describe('#constructor', function () {
    describe('when called with no coordinates', function () {
      it('should throw an Error', function () {
        expect(function () {
          new Location({name: 'Montreal'});
        }).to.throw(InvalidParameterError, / latitude /i);
      });
    });

    describe('when called with invalid coordinates', function () {
      it('should throw an Error', function () {
        expect(function () {
          new Location({name: 'Montreal forever', latitude: 10});
        }).to.throw(InvalidParameterError, / longitude /i);
      });
    });

    describe('when called with no name', function () {
      it('should throw an Error', function () {
        expect(function () {
          new Location({});
        }).to.throw(InvalidParameterError, /name /i);
      });
    });

    describe('when called with a name, lat and lng', function () {
      it('should work', function () {
        var location = new Location(MtlData);
        expect(location).to.be.an.instanceof(Location)
          .with.all.keys('name', 'latitude', 'longitude', 'displayName', 'comparableString', 'geoPoint');
        expect(location.geoPoint).to.be.an.instanceof(GeoPoint)
        expect(location.geoPoint).to.have.property('lat', MtlData.latitude);
        expect(location.geoPoint).to.have.property('lng', MtlData.longitude);
      });
    });
  });

  describe('#displayName', function () {
    var tests = [{
      desc: 'when only name is present',
      data: MtlData,
      expected: 'Montreal'
    }, {
      desc: 'when only name and country are present',
      data: _.extend({}, MtlData, {country: 'Canada'}),
      expected: 'Montreal, Canada'
    }, {
      desc: 'when only name and state are present',
      data: _.extend({}, MtlData, {state: 'QC'}),
      expected: 'Montreal, QC'
    }, {
      desc: 'when all proprerties are present',
      data: _.extend({}, MtlData, {state: 'QC', country: 'Canada'}),
      expected: 'Montreal, QC, Canada'
    }];
    
    tests.forEach(function (test) {
      describe(test.desc, function () {
        it('should properly generate a display name', function () {
          var location = new Location(test.data);
          expect(location.displayName).to.be.eql(test.expected);
        });
      });
    });
  });

  describe('#getScore', function () {
    

    describe('when the query does not contain a geoPoint', function () {
      describe('when there is a perfect match', function () {
        it('it should return 1', function () {
          var Montreal = new Location(MtlData);
          var query = new Query({q: 'Montreal'});
          expect(Montreal.getScore(query)).to.eql(1);
        });
        it('it should return 1 even if there are diactrics', function () {
          var Montreal = new Location(_.extend({}, MtlData, {name: 'Môntreàl'}));
          var query = new Query({q: 'Montréal'});
          expect(Montreal.getScore(query)).to.eql(1);
        });
      })
      describe('when there is a perfect mismatch', function () {
        it('it should return 0', function () {
          var Montreal = new Location(MtlData);
          var query = new Query({q: 'SomeRandomCityInTheMid'});
          expect(Montreal.getScore(query)).to.lt(0.1);

        });
      });

      describe('when there is some kind of match', function () {
        it('it should return 0', function () {
          var Montreal = new Location(MtlData);
          var query = new Query({q: 'Mtl'});
          expect(Montreal.getScore(query)).to.gt(0.14);
        });
      });

    });

    describe.skip('when the query does contain a getPoint', function () {});

  });


});
