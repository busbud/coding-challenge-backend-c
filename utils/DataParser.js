const path = require('path');
const { readFileSync } = require('fs');
const { getStateCode, countryCodeMap } = require('./StateCodes');

// This is the DataParser class
// it's main function is to load the tsv file supplied by a fileName to the constructor
// and return a JSON representation of that data

class DataParser {
  constructor(fileName, config = {}) {
    // we can pass desiredCountries and desiredPopulation as config params to help when parsing data
    const { desiredCountries, desiredPopulation } = config;
    this.desiredCountries = desiredCountries ? new Set(desiredCountries) : null;
    this.desiredPopulation = desiredPopulation ? desiredPopulation : Infinity;

    const filePath = path.resolve(__dirname, `../data/${fileName}`);
    const tsvData = readFileSync(filePath);
    this.data = this.convertTsvToJson(tsvData.toString());
  }

  // validate the object by checking the config object values
  validateObject = (obj = {}) => {
    if (!obj?.id) return false;
    if (this.desiredCountries && !this.desiredCountries.has(obj?.country)) return false;
    if (this.desiredPopulation && obj?.population < this.desiredPopulation) return false;
    return true;
  }

  convertTsvToJson = (tsv = "") => {
    const lines = tsv.split("\n");
    const result = [];
    const headers = lines[0].split("\t");

    for (let i = 1; i < lines.length; i++) {
      const obj = {};
      const currentline = lines[i].split("\t");

      for (let j = 0; j < headers.length; j++) {
        obj[headers[j]] = currentline[j];
      }

      if (this.validateObject(obj)) {
        result.push(this.deserializeObject(obj));
      }
    }
    return result;
  }

  deserializeObject = (obj = {}) => {
    // for the state (or province) we need to map the admin1 values to actual state/province codes
    const state = getStateCode(obj?.admin1);
    const country = countryCodeMap[obj?.country] ? countryCodeMap[obj?.country] : obj?.country;
    // here was actually want to use the ascii name to remove non-alpha numeric character (i.e: è)
    const name = `${obj?.ascii}, ${state}, ${country}`;

    return ({
      name,
      latitude: obj?.lat,
      longitude: obj?.long
    });
  }
}

module.exports = DataParser;