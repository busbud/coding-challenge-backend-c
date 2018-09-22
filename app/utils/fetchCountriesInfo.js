const fs = require('fs');

const {
    tsvToJson,
} = require('./parser');
const targets = require('../../config/targets');

const cache = {};

const fetchCountries = (res) => {
    if (cache.countries && Object.keys(cache.countries).length) {
        return cache.countries;
    }

    try {
        const data = fs.readFileSync('data/countryInfo.txt');
        const countriesArray = tsvToJson(
            Buffer.from(data).toString(),
            (res = {}) => targets.countries.includes(res.ISO)
        );

        cache.countries = {};
        // Make it a map to enable quick access from cities info
        countriesArray.forEach(({
            ISO,
            ...country
        }) => {
            cache.countries[ISO] = country;
        });

        return cache.countries;
    } catch (error) {
        console.log('Error utils:fetchCountriesInfo:fetchCountries', error);
        return res
            .status(500)
            .send({
                code: 'InternalServerError',
                message: error.toString(),
            });
    }
};

const fetchStates = (res) => {
    if (cache.states && Object.keys(cache.states).length) {
        return cache.states;
    }

    try {
        const data = fs.readFileSync('data/admin1CodesASCII.txt');
        const statesArray = tsvToJson(
            Buffer.from(data).toString(),
            (res = {}) => {
                const {code} = res;
                return targets.countries.includes(code.substring(0, 2));
            },
            ['code', 'name', 'nameAscii', 'geonameID']
        );

        cache.states = {};
        // Make it a map so we can quickly access states from city.admin1
        statesArray.forEach(({
            code,
            ...state
        }) => {
            cache.states[code] = state;
        });

        return cache.states;
    } catch (error) {
        console.log('Error utils:fetchCountriesInfo:fetchStates', err);
        return res
            .status(500)
            .send({
                code: 'InternalServerError',
                message: error.toString(),
            });
    }
};

const fetchCities = (res) => {
    if (cache.cities) {
        return cache.cities;
    }

    try {
        const data = fs.readFileSync('data/cities_canada-usa.tsv');
        cache.cities = tsvToJson(Buffer.from(data).toString(), (res = {}) => {
            const {
                population,
                country,
            } = res;
            return population &&
                population >= targets.minPopulation &&
                country &&
                targets.countries.includes(country);
        });

        return cache.cities;
    } catch (error) {
        console.log('Error utils:fetchCountriesInfo:fetchCities', err);
        return res
            .status(500)
            .send({
                code: 'InternalServerError',
                message: error.toString(),
            });
    }
};

module.exports = {
    fetchCountries,
    fetchStates,
    fetchCities,
};
