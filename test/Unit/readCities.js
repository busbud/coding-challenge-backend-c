const expect = require('chai').expect;
const { readCities } = require('../../src/readCities');

const sampleCitiesFilePath = 'test/unit/samples/10_cities.tsv';

describe('readCities function', function () {
    it('Returns a non-empty array of cities', function () {
        const cities = readCities(sampleCitiesFilePath);
        expect(cities.length).to.be.above(0);
    });

    it('Returns exact number of cities', function () {
        const cities = readCities(sampleCitiesFilePath);
        expect(cities.length).to.equal(10);
    });

    it('Returns city keys matching expectations', function () {
        const cities = readCities(sampleCitiesFilePath);

        const expectedKeys = [
            'id',
            'name',
            'ascii',
            'alt_name',
            'lat',
            'long',
            'feat_class',
            'feat_code',
            'country',
            'cc2',
            'admin1',
            'admin2',
            'admin3',
            'admin4',
            'population',
            'elevation',
            'dem',
            'tz',
            'modified_at',
        ];

        cities.forEach((city) => expect(city).to.all.keys(expectedKeys));
    });
});
