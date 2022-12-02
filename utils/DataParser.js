const path = require('path');
const { readFileSync } = require('fs');

class DataParser {
  constructor(fileName, config = {}) {
    const { desiredCountries, desiredPopulation } = config;
    this.desiredCountries = desiredCountries ? new Set(desiredCountries) : null;
    this.desiredPopulation = desiredPopulation ? desiredPopulation : Infinity;

    const filePath = path.resolve(__dirname, `../data/${fileName}`);
    const tsvData = readFileSync(filePath);
    this.data = this.convertToJSON(tsvData.toString());
  }

  validateObject = (obj = {}) => {
    if (!obj?.id) return false;
    if (this.desiredCountries && !this.desiredCountries.has(obj?.country)) return false;
    if (this.desiredPopulation && obj?.population < this.desiredPopulation) return false;
    return true;
  }

  convertToJSON = (tsv) => {
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
        result.push(this.serializeObject(obj));
      }
    }
    return result;
  }

  serializeObject = (obj = {}) => {
    return ({
      name: obj?.name,
      latitude: obj?.lat,
      longitude: obj?.long,
      score: 1
    });
  }
}

module.exports = DataParser;