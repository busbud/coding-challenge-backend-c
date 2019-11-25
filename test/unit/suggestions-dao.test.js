/* eslint-disable prefer-destructuring */
/* eslint-env mocha */

// Libraries
const Chai = require('chai');
const ChaiAsPromised = require('chai-as-promised');
const Sinon = require('sinon');

const SuggestionsRepository = require('../../dao/repositories/SuggestionsRepository');
const SuggestionsDao = require('../../dao/SuggestionsDao');
const SuggestionsDecorator = require('../../dao/SuggestionsDecorator');

// Mock data
const { rawSuggestions } = require('../mock-data');


Chai.use(ChaiAsPromised);
const assert = Chai.assert;
const sandbox = Sinon.createSandbox();

describe('SuggestionsDao', () => {
  afterEach(async () => {
    sandbox.restore();
  });

  describe('getSuggestions', () => {
    const search = { q: 'londo', latitude: 43.70011, longitude: -79.4163 };
    it('should return Suggestions from DB', async () => {
      sandbox.stub(SuggestionsRepository, 'getSuggestions').resolves({ suggestions: { suggestions: rawSuggestions } });

      const suggestionsDecoratorSpy = sandbox.spy(SuggestionsDecorator, 'decorate');
      const res = await SuggestionsDao.getSuggestions(this.ctx, search);
      const spiedValue = await suggestionsDecoratorSpy.returnValues[0];
      const expectedResult = {
        suggestions: spiedValue,
      };
      assert.deepEqual(res, expectedResult);
    });

    it('should return empty array for SomeRandomCityInTheMiddleOfNowhere', async () => {
      search.q = 'SomeRandomCityInTheMiddleOfNowhere';
      sandbox.stub(SuggestionsRepository, 'getSuggestions').resolves({ suggestions: [] });

      const suggestionsDecoratorSpy = sandbox.spy(SuggestionsDecorator, 'decorate');
      const res = await SuggestionsDao.getSuggestions(this.ctx, search);
      const spiedValue = await suggestionsDecoratorSpy.returnValues;
      const expectedResult = {
        suggestions: spiedValue,
      };
      assert.deepEqual(res, expectedResult);
    });
  });
});
