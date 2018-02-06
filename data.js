/**
 ** this module would be used to load the sample data provided
 **/

const fs = require('fs');
const d3 = require("d3-dsv");

const MINIMUM_POPULATION = 5000; //minmum population requiement

let loadData = (trie) => {
  console.log("Loading data..");
  fs.readFile('./data/cities_canada-usa.tsv', 'utf8', (err, contents) => { // read the input data file
    let data = d3.tsvParse(contents); // parse tsv to json object
    let cities = {};

    for (let i = 0; i < data.length; i++) {
      const cityInfo = data[i];
      if (cityInfo.population > MINIMUM_POPULATION) {
        let cityName = (cityInfo.name + " , " + cityInfo.admin1 + " , " + cityInfo.country).toLowerCase();
        let city = {
          name: cityInfo.name + " , " + cityInfo.admin1 + " , " + cityInfo.country,
          latitude: cityInfo.lat,
          longitude: cityInfo.long,
          score: 0
        };
        cities[cityName] = city; //building the json object
      }
    }
    trie.addFromObject(cities); // add to the ds
  });
}

exports.loadData = loadData;