import { citiesTsv, expectedMockedArray, expectedProvinceObject, provincesTsv } from '../fixtures/cities.fixture';
import Cities from '../../src/services/cities';
import { expect } from 'chai';
import fs from 'fs';
import { stub } from 'sinon';

describe('Cities Service', () => {
  let citiesService;

  describe('when creating valid array of cities', () => {
    let mockFs;

    before(() => {
      mockFs = stub(fs, 'readFileSync').returns(citiesTsv);
      citiesService = new Cities();
    });

    after(() => {
      mockFs.restore();
    });

    it('should return valid array of cities', () => {
      expect(citiesService.createCitiesArray()).to.deep.equal(expectedMockedArray);
    });
  });

  describe('when creating valid object mapping provinces', () => {
    let mockFs;

    before(() => {
      mockFs = stub(fs, 'readFileSync').returns(provincesTsv);
      citiesService = new Cities();
    });

    after(() => {
      mockFs.restore();
    });

    it('should return valid object of provinces', () => {
      expect(citiesService.createProvinceList()).to.deep.equal(expectedProvinceObject);
    });
  });
});
