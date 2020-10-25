const fs = require('fs');
const path = require('path');
const util = require('util');
const readline = require('readline');

const stat = util.promisify(fs.stat);

const DEFAULT_MIN_POPULATION = 5000; // minmum population requiement

const COL = {
  GEONAMEID: 0, // integer id of record in geonames database
  NAME: 1, // name of geographical point (utf8) varchar(200)
  ASCIINAME: 2, // name of geographical point in plain ascii characters, varchar(200)
  ALTERNATENAMES: 3, // alternatenames, comma separated varchar(5000)
  LATITUDE: 4, // latitude in decimal degrees (wgs84)
  LONGITUDE: 5, // longitude in decimal degrees (wgs84)
  FEATURE_CLASS: 6, // see http://www.geonames.org/export/codes.html, char(1)
  FEATURE_CODE: 7, // see http://www.geonames.org/export/codes.html, varchar(10)
  COUNTRY_CODE: 8, // ISO-3166 2-letter country code, 2 characters
  CC2: 9, // alternate country codes, comma separated, ISO-3166 2-letter country code, 60 characters
  ADMIN1_CODE: 10, // fipscode (subject to change to iso code), see exceptions below, see file admin1Codes.txt for display names of this code; varchar(20)
  ADMIN2_CODE: 11, // code for the second administrative division, a county in the US, see file admin2Codes.txt; varchar(80)
  ADMIN3_CODE: 12, // code for third level administrative division, varchar(20)
  ADMIN4_CODE: 13, // code for fourth level administrative division, varchar(20)
  POPULATION: 14, // bigint (8 byte int)
  ELEVATION: 15, // in meters, integer
  DEM: 16, // digital elevation model, srtm3 or gtopo30, average elevation of 3''x3'' (ca 90mx90m) or 30''x30''
  TIMEZONE: 17, // the timezone id (see file timeZone.txt) varchar(40)
  MODIFICATION_DATE: 18, // date of last modification in yyyy-MM-dd format
};

const CANADA_PROVINCES_CODES = {
  '01': 'AB',
  '02': 'BC',
  '03': 'MB',
  '04': 'NB',
  '05': 'NL',
  '07': 'NS',
  '08': 'ON',
  '09': 'PE',
  '10': 'QC',
  '11': 'SK',
  '12': 'YT',
  '13': 'NT',
  '14': 'NU',
};

// load data
const loadData = async (fileName = 'cities_canada-usa.tsv', trie, config) => {
  const filePath = path.resolve(__dirname, '../data', fileName);
  const stats = await stat(filePath);
  const minPopulation = config.min_city_population || DEFAULT_MIN_POPULATION;

  if (!stats.isFile()) {
    throw Error(`Cannot load data file: ${filePath} is not a regular file`);
  } else {
    const lineReader = readline.createInterface({
      input: fs.createReadStream(filePath),
    });

    return new Promise((resolve, reject) => {
      lineReader.on('line', (line) => {
        const currentRow = line.split('\t');
        const stateOrProv = currentRow[COL.COUNTRY_CODE] === 'CA' ? CANADA_PROVINCES_CODES[currentRow[COL.ADMIN1_CODE]] : currentRow[COL.ADMIN1_CODE];
        const city = {
          id: `${currentRow[COL.GEONAMEID]}__${currentRow[COL.ASCIINAME]}__${stateOrProv}__${currentRow[COL.COUNTRY_CODE]}`.toLowerCase().replace(/ /g, '_'),
          name: currentRow[COL.ASCIINAME],
          displayName: `${currentRow[COL.NAME]}, ${stateOrProv}, ${currentRow[COL.COUNTRY_CODE]}`,
          latitude: currentRow[COL.LATITUDE],
          longitude: currentRow[COL.LONGITUDE],
          country: currentRow[COL.COUNTRY_CODE],
          population: parseInt(currentRow[COL.POPULATION], 10) || null,
          score: 0,
        };
        // datafile has already been filtered and doesn't include any city with pop. below 5000
        // however I included the validation because restricting was clearly indicated as a requirement
        if (city.population > minPopulation) {
          // Add the city to the trie using different keys:
          // first the city's name and then each altname
          const cityByKeys = {};
          cityByKeys[currentRow[COL.ASCIINAME].toLowerCase().replace(/ /g, '')] = city;
          currentRow[COL.ALTERNATENAMES].split(',').forEach((altname) => {
            cityByKeys[altname.toLowerCase().replace(/ /g, '')] = city;
          });
          trie.addFromObject(cityByKeys);
        }
      });
      lineReader.on('close', resolve);
      lineReader.on('error', reject);
    });
  }
}
exports.loadData = loadData;
