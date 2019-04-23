const chai = require('chai');
const expect = chai.expect;
chai.use(require('chai-things'));

const load_data = require('../sync-load-data');

describe('load-data', () => {
  it('should load the entire data set', () => {
    const LINES = 7237; // number of lines of data of ../data/cities_canada-usa.tsv, minus the header line
    expect(load_data).to.be.an('array');
    expect(load_data).to.have.lengthOf(LINES);
  });
});
