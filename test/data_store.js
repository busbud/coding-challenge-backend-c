
var expect  = require('chai').expect;
var data_store = require('../lib/data_store');
var path = require('path');


/*global describe, before, it */
describe('Setting the data store from a string', function () {
  before(function (done) {
    var CSV_STRING = 'id,ascii,lat,long,country,admin1,population\n' +
                     '1,foo,1,1,US,CA,5500\n' +
                     '2,bar,2,2,CA,10,6000\n';

    data_store.setDataSource({string: CSV_STRING}, done);
  });

  it('Looking up partial substring', function () {
    data_store.query('f', function (err, result) {
      // console.log('result: %s', JSON.stringify(result, null, 4));
      expect(err).to.equal(null);
      expect(result).to.have.length(1);
    });
  });

});


describe('Setting the data store from a file', function () {
  before(function (done) {
    data_store.setDataSource({file: path.resolve(__dirname, '../data/cities_canada-usa.tsv')}, done);
  });

  it('Looking up partial substring', function () {
    data_store.query('Montreal', function (err, result) {
      // console.log('result: %s', JSON.stringify(result, null, 4));
      expect(err).to.equal(null);
      expect(result).to.have.length(2);
    });
  });

});
