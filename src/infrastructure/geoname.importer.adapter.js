const fs = require('fs');
const readline = require('readline');
const countryAdapter = require('./country.adapter');
const divisionsAdapter = require('./administrative.division.adapter');

const lineColumns = [
  'geonameid',
  'name',
  'asciiname',
  'alternatenames',
  'latitude',
  'longitude',
  'feature_class',
  'feature_code',
  'country_code',
  'cc2',
  'admin1_code',
  'admin2_code',
  'admin3_code',
  'admin4_code',
  'population',
  'elevation',
  'dem',
  'timezone',
  'modification_date',
];
const buildSuggestion = async (parsedLine) => new Promise((resolve, reject) => {
  (async () => {
    const country = await countryAdapter.findByCode(parsedLine.country_code)
      .catch((reason) => reject(reason));

    const divisionCode = await divisionsAdapter
      .findCodeByFipsCode(parsedLine.country_code, parsedLine.admin1_code)
      .catch((reason) => reject(reason));

    const name = parsedLine.asciiname;

    resolve({
      id: parsedLine.geonameid,
      fullSuggestion: `${name}, ${divisionCode}, ${country.display}`,
      name,
      division: divisionCode,
      country,
      location: {
        lat: parsedLine.latitude,
        lon: parsedLine.longitude,
      },
    });
  })();
});

const lineToObject = (line) => {
  const lineValues = line.split('\t');
  const result = {};
  // eslint-disable-next-line no-plusplus
  for (let i = 0; i < lineColumns.length; i++) {
    const key = lineColumns[i];
    result[key] = lineValues[i] || null;
  }

  return result;
};

const performImport = (filepath, suggestionCallback) => new Promise((resolve) => {
  const reader = readline.createInterface({
    input: fs.createReadStream(filepath),
    crlfDelay: Infinity,
  });

  const callbacks = [];
  reader.on('line', async (line) => {
    const parsedLine = lineToObject(line);
    if (parsedLine.geonameid !== 'id') {
      console.log(`Processing suggestion ${parsedLine.geonameid}`);
      const suggestion = await buildSuggestion(parsedLine).catch((reason) => console.log(reason));
      callbacks.push(suggestionCallback(suggestion));
    }
  });

  reader.on('close', () => {
    Promise.all(callbacks).then(() => {
      console.log('Import finished');
      resolve();
    })
      .catch((reason) => console.log(reason));
  });
});

module.exports = performImport;
