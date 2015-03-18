var store = require('../../lib/store');
var Query = require('../../lib/query');

var path = require('path');
var expect  = require('chai').expect;

var validTestPath = path.join(__dirname, '../../data/test_cities.tsv');

describe('store', function() {
  beforeEach(function () {
    store._reset();
  });

  describe('#connect', function () {
    describe('when the data are not reachable (file does not exist)', function () {
      it('should call the callback with an error', function (done) {
        store.connect('./this/file/does/not/exist.tsv', function (err) {
          expect(err).to.be.an.instanceof(Error);
          done();
        });
      });
    });

    describe('when loaded properly', function () {
      it('should call the callback with no error', function (done) {
        store.connect(validTestPath, function (err) {
          expect(err).to.be.null;
          done();
        });
      });
    });
  });
});
