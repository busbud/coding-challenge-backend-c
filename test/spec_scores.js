const expect = require('chai').expect;
const score = require('../src/scores');


describe('the levenshtein distance function', () => {

  it('should work for a perfect match', () => {
    const edit_distance = score.levenshtein('levenshtein', 'levenshtein');
    expect(edit_distance).to.equal(0);
  });

  it('should work for \'kitten\' and \'sitten\'', () => {
    const edit_distance = score.levenshtein('kitten', 'sitten');
    expect(edit_distance).to.equal(1);
  });

  it('should work for \'kitten\' and \'sitting\'', () => {
    const edit_distance = score.levenshtein('kitten', 'sitting');
    expect(edit_distance).to.equal(3);
  });

  it('should work for \'kitten\' and \'ten\'', () => {
    const edit_distance = score.levenshtein('kitten', 'ten');
    expect(edit_distance).to.equal(3);
  });

  it('should work for \'kitten\' and \'\'', () => {
    const edit_distance = score.levenshtein('kitten', '');
    expect(edit_distance).to.equal(6);
  });

  it('should work for \'\' and \'smurf\'', () => {
    const edit_distance = score.levenshtein('', 'smurf');
    expect(edit_distance).to.equal(5);
  });

});


describe('the haversine proximity function', () => {

  it('should return the correct distance between Toronto and Montreal', () => {
    const distance = score.proximity({
      lat: 43.70011,
      lng: -79.4163
    }, {
      lat: 45.50884,
      lng: -73.58781
    });

    expect(Math.floor(distance)).to.equal(503);
  });

  it('should return the correct distance between Edmonton and Calgary', () => {
    const distance = score.proximity({
      lat: 53.5333,
      lng: -113.5000
    }, {
      lat: 51.0486,
      lng: -114.0708
    });

    expect(Math.floor(distance)).to.equal(278);
  });

  it('should return the correct distance between Victoria and Vancouver', () => {
    const distance = score.proximity({
      lat: 48.4222,
      lng: -123.3657
    }, {
      lat: 49.2827,
      lng: -123.1207
    });

    expect(Math.floor(distance)).to.equal(97);
  });

  it('should return the correct distance between Moscow and London', () => {
    const distance = score.proximity({
      lat: 55.7500,
      lng: 37.6167
    }, {
      lat: 51.5072,
      lng: 0.1275
    });

    expect(Math.floor(distance)).to.equal(2484);
  });

});