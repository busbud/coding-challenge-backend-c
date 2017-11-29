import ArgChecker from './ArgChecker';
const expect = require('chai').expect;

describe('with a valid query', () => {
  it('should return true when query contains only city name', () => {
    // given
    const query = { q: 'montreal' };
    const checker = new ArgChecker();

    // when
    const result = checker.check(query);

    // then
    expect(result).to.be.true;
  });

  it('should return true when query contains city name, latitude and longitude', () => {
    // given
    const query = { q: 'montreal', latitude: 45.45286, longitude: -73.64918 };
    const checker = new ArgChecker();

    // when
    const result = checker.check(query);

    // then
    expect(result).to.be.true;
  });

  it('should return true when query contains latitude and longitude', () => {
    // given
    const query = { latitude: 45.45286, longitude: -73.64918 };
    const checker = new ArgChecker();

    // when
    const result = checker.check(query);

    // then
    expect(result).to.be.true;
  });
});

describe('with an invalid query', () => {
  it('should return false when query contains empty parameters', () => {
    // given
    const query = {};
    const checker = new ArgChecker();

    // when
    const result = checker.check();

    // then
    expect(result).to.be.false;
  });

  it('should return false when query contains no parameters', () => {
    // given
    const checker = new ArgChecker();

    // when
    const result = checker.check();

    // then
    expect(result).to.be.false;
  });

  it('should return false when query contains only latitude', () => {
    // given
    const query = { latitude: 45.45286 };
    const checker = new ArgChecker();

    // when
    const result = checker.check(query);

    // then
    expect(result).to.be.false;
  });

  it('should return false when query contains longitude', () => {
    // given
    const query = { longitude: -73.64918 };
    const checker = new ArgChecker();

    // when
    const result = checker.check(query);

    // then
    expect(result).to.be.false;
  });
});
