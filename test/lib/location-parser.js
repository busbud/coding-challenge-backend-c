var locationParser = require('../../lib/location-parser');
var Location = require('../../lib/location');

var path = require('path');
var _ = require('lodash');
var expect  = require('chai').expect;

describe('location-parser module', function() {
  describe('#convert', function () {
    var defaultRaw = {
      ascii: 'city name',
      lat: 10,
      long: 11
    }
    var tests = [{
      key: 'country',
      raw: {
        country: 'US'
      },
      parsed: {
        country: 'USA'
      }

    }, {
      key: 'country',
      raw: {
        country: 'CA'
      },
      parsed: {
        country: 'Canada'
      }
    }, {
      key: 'state',
      raw: {
        country: 'CA',
        admin1: '01'
      },
      parsed: {
        state: 'AB'
      }
    }];

    describe('with valid data', function () {
      tests.forEach(function(test, index) {
        it('should return properly parsed location (' + (index + 1) + '- ' + test.key + ')', function (done) {
          locationParser.convert(_.extend(test.raw, defaultRaw), function(err, location) {
            expect(err).to.be.null;
            expect(location[test.key]).to.eql(test.parsed[test.key]);
            done();
          })
        });
      });
    });
  });

  describe('#transorm', function () {
    describe('with valid data', function (done) {
      var locations;
      before(function (done) {
        locationParser.load(path.join(__dirname, '../../data/test_cities.tsv'), function (err, locs) {
          if (err) {
            done(err);
          }
          locations = locs;
          done();
        });
      });
      it('should return expected properties', function () {
        expect(locations).to.be.instanceof(Array);
        expect(locations).to.have.length(4);
        locations.forEach(function (location) {
          expect(location).to.be.an.instanceof(Location);
          expect(location).to.have.all.keys(
            'id', 'geoPoint', 'country', 'state', 'displayName',
            'name', 'comparableString', 'latitude', 'longitude');
        });
      });
    });
  });
});
