const { buildSuggestions } = require('../../src/suggestionGeneration');

var expect = require('chai').expect;

describe('Suggestion generation function', function () {
    it('Never returns cities where population is not above 5000', function () {
        const cities = [{ name: 'name', population: 5000 }];

        const suggestions = buildSuggestions(cities, { searchText: 'name' });

        expect(suggestions.length).to.equal(0);
    });
});
