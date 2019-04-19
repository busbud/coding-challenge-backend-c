const fs = require('fs');
const d3 = require('d3-dsv');

const filterDataByPopulation = citiesData => citiesData.filter(cityData => cityData.population >= 5000);
const filterDataByCountry = citiesData => citiesData.filter(cityData => ['CA', 'US'].includes(cityData.country));
const filterByPopAndByCountry = citiesData => filterDataByPopulation(filterDataByCountry(citiesData));
const sortDataByPopulation = citiesData => citiesData.sort((cityDataA, cityDataB) => cityDataB.population - cityDataA.population);

let stringData = fs.readFileSync('./data/cities_canada-usa.tsv', 'utf8');
stringData = stringData.replace(/["]/g, ''); //Force data to conform to RFC 4180 before parsing it
module.exports = sortDataByPopulation(filterByPopAndByCountry(d3.tsvParse(stringData, d3.autoType)));
