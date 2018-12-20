const fs = require('fs');
const path = require('path');

const csv = require('csv-parser');

const City = require('../../models/city');


const fips = {
  '01': 'AB',
  '02': 'BC',
  '03': 'MB',
  '04': 'NB',
  '05': 'NL',
  '07': 'NS',
  '13': 'NT',
  '14': 'NU',
  '08': 'ON',
  '09': 'PE',
  '10': 'QC',
  '11': 'SK',
  '12': 'YT',
};

const countries = {
  'CA': 'Canada',
  'US': 'USA',
};

/**
 * Parses cities
 *
 * @returns {Promise} Promise object represents the needed cities data
 */
function parseCitiesFromFile () {
  return new Promise(resolve => {
    const filePath = path.join(__dirname, 'data', 'cities_canada-usa.tsv');

    const results = [];
    fs.createReadStream(filePath)
      .pipe(csv({separator: '\t'}))
      .on('data', (data) => results.push(data))
      .on('end', () => {
        const filteredResults = results.filter(
          result => Object.keys(countries).includes(result.country)
        );
        const cities = filteredResults.map(result => {
          const {id, name, country: countryCode, admin1, lat, long} = result;
          return {
            id,
            name,
            admin_division_code: countryCode === 'CA' ? fips[admin1] : admin1,
            country: countries[countryCode],
            lat,
            long,
          };
        });
        resolve(cities);
      });
  });
}

/**
 * Allows to import cities in database
 */
async function importCities () {
  await City.sync({force: true});

  const parsedCities = await parseCitiesFromFile();
  try {
    await City.bulkCreate(parsedCities, {validate: true});
  } catch (e) {
    console.log('Oops! An error has occured:', e.message);
    process.exit(1);
  }

  // Success message
  console.log('\x1b[32m%s\x1b[0m', `Imported ${parsedCities.length} cities.`);
  process.exit(0);
}

importCities();
