const fs = require('fs');
const d3 = require('d3-dsv');
const dataUtils = require('./data-utils');

let stringData = fs.readFileSync('./data/cities_canada-usa.tsv', 'utf8');
stringData = stringData.replace(/["]/g, ''); //Force data to conform to RFC 4180 before parsing it
module.exports = dataUtils.sortDataByPopulation(dataUtils.filterByPopAndByCountry(dataUtils.dropUnusedDataFields(d3.tsvParse(stringData, d3.autoType))));
