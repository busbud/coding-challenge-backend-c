import * as fs from 'fs';
import * as readline from 'readline';
import {tabbedGeoDataToObject} from './tsv_utils';
import {ICityRawData} from '../../interfaces/interfaces';

/** Parses tsv file to array of ICityRawData objects */
async function parseCitiesDataFile(fileLocation: string): Promise<ICityRawData[]> {
  const cities: ICityRawData[] = [];
  const rl = readline.createInterface({
    input: fs.createReadStream(fileLocation),
    crlfDelay: Infinity,
  });

  return new Promise((resolve) => {
    rl.on('line', (line) => {
      const city = tabbedGeoDataToObject(line.split('\t'));
      cities.push(city);
    });

    rl.on('close', () => {
      console.log('Finished reading data file');
      resolve(cities);
    });
  });
}

/** Get cities from certain countries */
export async function getCitiesDataFromFile(countries: string[]): Promise<ICityRawData[]> {
  const file = './data/cities_canada-usa.tsv';

  const cities = await parseCitiesDataFile(file);
  return filterCitiesByCountries(cities, countries);
}

function filterCitiesByCountries(cities: ICityRawData[], countryCodes: string[]): ICityRawData[] {
  const countryCodeSet = new Set(countryCodes);
  return cities.filter((c) => countryCodeSet.has(c.country));
}
