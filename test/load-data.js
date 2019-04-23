const chai = require('chai');
const expect  = chai.expect;
chai.use(require('chai-things'));

const loadData = require('../sync-load-data');

describe('load-data', () => {
  it('should load the entire data set', () => {
    const lines = 7237; //number of lines of data of ../data/cities_canada-usa.tsv, minus the header line
    expect(loadData).to.be.an('array');
    expect(loadData).to.have.lengthOf(lines);
  });
});