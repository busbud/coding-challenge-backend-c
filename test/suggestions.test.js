var assert = require('chai').assert;
var suggestions = require('../src/suggestions');

describe('suggestions unit tests', function () {
    describe('Without lat/long', function () {
        it('Filter on name only - first letter', async () => {
            // check with first load
            const cities = await suggestions.getSuggestions('A');
            assert.isTrue(0 < cities.length);
        });
        it('Filter on name only - more letter', async () => {
            // check with first load
            const cities = await suggestions.getSuggestions('Abb');
            assert.isTrue(0 < cities.length);
        });
        it('Filter on name only - with valid city', async () => {
            // check with first load
            const cities = await suggestions.getSuggestions('Montreal');
            assert.equal('Montréal, 10, Canada', cities[0].name)
            assert.equal(1, cities[0].score)
        });
    });
    describe('With lat/long', function () {
        const latitude = 45.50884;
        const longitude = -73.58781;
        it('Filter on name only - first letter', async () => {
            // check with first load
            const cities = await suggestions.getSuggestions('A', latitude, longitude);
            assert.isTrue(0 < cities.length);
        });
        it('Filter on name only - more letter', async () => {
            // check with first load
            const cities = await suggestions.getSuggestions('Abb');
            assert.isTrue(0 < cities.length);
        });
        it('Filter on name only - montréal is first', async () => {
            // check with first load
            const cities = await suggestions.getSuggestions('Montreal', latitude, longitude);
            assert.equal('Montréal, 10, Canada', cities[0].name)
            assert.equal(1, cities[0].score)
        });
        it('Filter on name only - montréal ouest is first', async () => {
            // check with first load
            const cities = await suggestions.getSuggestions('Montreal', 45.45286, -73.65918);
            assert.equal('Montréal-Ouest, 10, Canada', cities[0].name)
        });
    });
});
