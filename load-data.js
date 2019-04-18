const fs = require('fs');
const LineStream = require('byline').LineStream;
const pump = require('pump');
const d3 = require('d3-dsv');

module.exports = path => new Promise( (resolve, reject) => {
  let citiesData = [];
  const citiesDataStream = fs.createReadStream('./data/cities_canada-usa.tsv', 'utf8');
  const lineStream = new LineStream();

  lineStream.on('data', line => {
    let rows = d3.tsvParseRows(line, d3.autoType);
    citiesData.push(...rows);
  });
  pump(citiesDataStream, lineStream, err => {
    if(!err) {
      let headerArray = citiesData.shift();
      let mappedCitesData = citiesData.map(cityValuesArray => {
        let cityData = {};
        cityValuesArray.forEach( (value, columnIndex) => cityData[headerArray[columnIndex]] = value);
        return cityData;
      });
      let filteredCitiesData = mappedCitesData.filter(cityData => cityData.population >= 5000);
      resolve(filteredCitiesData);
    } else {
      reject(err)
    }
  });
});
