const expect = require('chai').expect;
const parser = require('../src/parser');
const config = require('../config.json');

const test_file = config.city_file;


describe('the city parser', () => {

  it('should return 7238 results', (done) => {
    parser(test_file, (err, data) => {
      expect(data.length).to.equal(7237);
      done();
    });
  });

  it('should return an error when no file is given', (done) => {
    parser(null, (err, data) => {
      expect(data).to.equal(undefined);
      expect(err).to.deep.equal(new Error('Filename cannot be null, undefined or \'\''));
      done();
    });
  });

});