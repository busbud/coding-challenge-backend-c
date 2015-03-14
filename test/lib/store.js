var store = require('../../lib/store');
var Query = require('../../lib/query');

var path = require('path');
var expect  = require('chai').expect;

describe('store', function() {

  describe('#load', function () {
  describe('when loaded properly', function () {
    it('should work... obviously...', function (done) {
      store.load(path.join(__dirname, '../../data/test_cities.tsv'), function (err, locs) {
        store.find(new Query({q: 'Acton Vale'}), function (err, results) {
          done();
        });     
      });

    });
  });
  });
});
