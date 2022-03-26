const { buildSuggestions } = require('../../src/suggestionGeneration');

var expect = require('chai').expect;

describe('Suggestion generation function', function () {
    it('Never returns cities where population is not above 5000', function () {
        const cities = [{ name: 'name', population: 5000 }];

        const suggestions = buildSuggestions(cities, { searchText: 'name' });

        expect(suggestions.length).to.equal(0);
    });

    it('Returns suggestions in relevancy order', function () {
        const cities = [
            { name: 'name-with-a-worse-match', population: 5001 },
            { name: 'name', population: 5001 },
        ];

        const suggestions = buildSuggestions(cities, { searchText: 'name' });

        expect(suggestions.length).to.equal(2);
        expect(suggestions[0].name).to.satisfy((name) =>
            name.startsWith('name,')
        );
    });

    it('Location affects relevancy order', function () {
        const closeLocation = {
            latitude: 40.7128,
            longitude: 74.006,
        };

        const farLocation = {
            latitude: 50.7128,
            longitude: 74.006,
        };

        const cities = [
            { name: 'name-a', population: 5001, ...farLocation },
            { name: 'name-b', population: 5001, ...closeLocation },
        ];

        const suggestions = buildSuggestions(cities, {
            searchText: 'name',
            ...closeLocation,
        });

        expect(suggestions.length).to.equal(2);
        expect(suggestions[0].name).to.satisfy((name) =>
            name.startsWith('name-b,')
        );
    });
});
