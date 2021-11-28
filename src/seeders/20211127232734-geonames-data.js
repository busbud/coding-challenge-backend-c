const csv = require('csv-parser');
const fs = require('fs');

// Geonames administrative division canada map (https://www.geonames.org/CA/administrative-division-canada.html)
const provinceMap = {
  '01': 'AB',
  '02': 'BC',
  '03': 'MB',
  '04': 'NB',
  '05': 'NL',
  '07': 'NS',
  '08': 'NU',
  '09': 'ON',
  '10': 'PE',
  '11': 'QC',
  '12': 'SK',
  '13': 'YT',
};

module.exports = {
  up: async (queryInterface) => {
    const csvFilePath = 'data/cities_canada-usa.tsv';
    const cistiesDataArray = [];

    const geoData = await new Promise(function (resolve) {
      fs.createReadStream(csvFilePath)
        .pipe(csv({ separator: '\t' }))
        .on('data', (data) => {
          const stateOrProvince = provinceMap[data.admin1]
            ? provinceMap[data.admin1]
            : data.admin1;

          cistiesDataArray.push({
            name: data.name,
            country_code: data.country,
            state: stateOrProvince,
            latitude: data.lat ? parseFloat(data.lat) : null,
            longitude: data.long ? parseFloat(data.long) : null,
            population: data.population ? parseInt(data.population) : null,
            createdAt: new Date(Date.now()),
            updatedAt: new Date(Date.now()),
          });
        })
        .on('end', () => {
          resolve(cistiesDataArray);
        });
    });

    await queryInterface.bulkInsert('geonames', geoData, {});
  },

  down: async (queryInterface) => {
    await queryInterface.bulkDelete('geonames', null, {});
  },
};
