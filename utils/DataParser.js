const path = require('path');
const { readFileSync } = require('fs');
const { getStateCode, countryCodeMap } = require('./StateCodes');

class DataParser {
  constructor(fileName, config = {}) {
    const { desiredCountries, desiredPopulation } = config;
    this.desiredCountries = desiredCountries ? new Set(desiredCountries) : null;
    this.desiredPopulation = desiredPopulation ? desiredPopulation : Infinity;

    const filePath = path.resolve(__dirname, `../data/${fileName}`);
    const tsvData = readFileSync(filePath);
    this.data = this.convertTsvToJson(tsvData.toString());
  }

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
    const state = getStateCode(obj?.admin1);
    const country = countryCodeMap[obj?.country] ? countryCodeMap[obj?.country] : obj?.country;
    const name = `${obj?.ascii}, ${state}, ${country}`;

    return ({
      name,
      latitude: obj?.lat,
      longitude: obj?.long
    });
  }
}

module.exports = DataParser;