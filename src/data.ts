import * as csv from 'fast-csv';
import * as fs from 'fs';
import * as path from 'path';

/**
 * Defines only the fields we care about in the TSV file
 */
interface CSVCity {
  long: string;
  lat: string;
  population: string;
  ascii: string;
  country: string;
}

/**
 * Defines the parsed data from TSV file
 */
export interface City {
  name: string;
  longitude: number;
  latitude: number;
  population: number;
  country: string;
}

/**
 * List of cities parsed from TSV file
 */
const cities: City[] = [];

const filePath = path.resolve(__dirname, '..', 'data', 'cities_canada-usa.tsv');

/**
 * Promise that executes once & returned on ever loadCities function call
 */
const citiesPromise: Promise<City[]> = new Promise<City[]>((resolve, reject) => {
  fs.createReadStream(filePath, { encoding: 'utf-8' })
    .pipe(csv.parse({ delimiter: '\t', headers: true, ignoreEmpty: true, objectMode: true, quote: null }))
    .transform((row: CSVCity, next) => {
      if (!!row) {
        const { lat, long, population, ascii, country } = row;
        next(null, {
          name: ascii,
          longitude: parseFloat(long),
          latitude: parseFloat(lat),
          population: parseInt(population, 10),
          country,
        });
      } else {
        next(new Error('Unable to parse row'))
      }
    })
    .on('error', err => reject(err))
    .on('data', (city) => {
      if (city.population > 5000) {
        cities.push(city)
      }
    })
    .on('end', () => resolve(cities))
  ;
});

/**
 * Returns parsed list of cities
 */
export function loadCities(): Promise<City[]> {
  return citiesPromise;
}
