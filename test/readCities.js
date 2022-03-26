var expect = require('chai').expect;
const { readCities } = require('../src/readCities');

const sampleCitiesFilePath = 'test/samples/10_cities.tsv';

describe('readCities', function () {
    it('Returns a non-empty array of cities', function () {
        const cities = readCities(sampleCitiesFilePath);
        expect(cities.length).to.be.above(0);
    });

    it('Returns exact number of cities', function () {
        const cities = readCities(sampleCitiesFilePath);
        expect(cities.length).to.equal(10);
    });
});
