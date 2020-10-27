/* eslint-disable func-names, prefer-arrow-callback */

const { expect } = require('chai');

const { createCityMock } = require('../mocks');
const { filterCities } = require('../../../src/helpers');

describe('Test the miscellaneous helper functions', function () {
  describe('Test the filterCities function', function () {
    it('Must return a list of filtered cities', function () {
      // Arrange
      const cities = [createCityMock()];
      const response = {
        name: 'London',
        latitude: '42.98339',
        longitude: '-81.23304',
        score: 1,
      };

      // Atc
      const suggestions = filterCities({
        cities,
        name: 'Londo',
        latitude: 42.98339,
        longitude: -81.23304,
      });

      // Asset
      expect(suggestions).to.be.instanceof(Array);
      expect(suggestions).to.have.deep.members([response]);
    });

    it('Must return an empty array', function () {
      // Arrange
      const cities = [];

      // Atc
      const suggestions = filterCities({
        cities,
        name: 'Londo',
        latitude: 42.98339,
        longitude: -81.23304,
      });

      // Asset
      expect(suggestions).to.be.instanceof(Array);
      expect(suggestions).to.have.lengthOf(0);
    });
  });
});
