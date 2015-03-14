var locationParser = require('../../lib/location-parser');
var Location = require('../../lib/location');

var path = require('path');
var expect  = require('chai').expect;

describe('location-parser module', function() {
  describe.skip('#convert', function () {
    var tests = [{
      raw: {},
      parsed: {}
    }, {

    }];

    describe('with valid data', function () {
      tests.forEach(function(test, index) {
        it('should return properly parsed location (' + (index + 1) + ')', function (done) {
          locationParser.convert(test.raw, function(err, data) {
            expect(err).to.be.null;
            expect(data).to.eql(test.parsed);
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
