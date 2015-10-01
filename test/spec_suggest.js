const expect = require('chai').expect;
const suggest = require('../src/suggest');
const parser = require('../src/parser');
const config = require('../config.json');


var cities = [];

before((done) => {
  parser(config.city_file, (err, data) => {
    cities = data;
    done();
  });
});

describe('the suggestion function', () => {

  it('should return 10 results by default', () => {
    expect(suggest(cities, '').length).to.equal(10);
  });


  it('should return results in descending score order', () => {
    const suggestions = suggest(cities, 'mo');
    expect(suggestions[0].score).to.be.above(suggestions[1].score);
    expect(suggestions[1].score).to.be.above(suggestions[2].score);
    expect(suggestions[2].score).to.be.above(suggestions[3].score);
    expect(suggestions[3].score).to.be.above(suggestions[4].score);
    expect(suggestions[4].score).to.be.above(suggestions[5].score);
  });

});