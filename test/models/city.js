import chai, { expect } from 'chai';
import chaiAlmost from 'chai-almost';

import { City } from '../../src/models';

chai.use(chaiAlmost(0.5));

const distanceTestCases = [
  {
    originalPoint: {
      lat: 42.98339,
      long: -81.23304
    },
    result: 0
  },
  {
    originalPoint: {
      lat: 42.18339,
      long: -81.23304
    },
    result: 89
  },
  {
    originalPoint: {
      lat: 42.98339,
      long: -81.33304
    },
    result: 8
  }
];

describe('City', () => {
  let city;

  beforeEach(() => {
    city = new City({
      name: 'London',
      lat: 42.98339,
      long: -81.23304
    });
  });

  it('#name', () => {
    expect(city.name).to.equal('London');
  });

  it('#match', () => {
    expect(city.match(/California/)).to.be.null;
    expect(city.match(/London/)).to.be.equal(1);
    expect(city.match(/Lon/)).to.be.equal(0.5);
  });

  it('#distanceFrom', () => {
    distanceTestCases.forEach((testDatum) => {
      expect(city.distanceFrom(testDatum.originalPoint)).to.almost.equal(testDatum.result);
    });
  });
});
