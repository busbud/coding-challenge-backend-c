#!/usr/bin/env node -r @babel/register
import yargs from 'yargs';
import path from 'path';
import { filter, map } from 'rxjs/operators';

import SearchCityFromEs from '../SearchCityFromEs';
import { TsvParser } from '../../../utils';
import elasticSearchClient from '../connection';

const loadIndex = async (dataFile, elSearch) => {
  const tsvParser = new TsvParser({ filePath: dataFile });
  const observable = tsvParser.parse()
    .pipe(
      filter(data => Number(data.population) > 5000),
      map((city) => {
        const { lat, long: lon } = city;
        const location = `${lat}, ${lon}`;
        return {
          ...city,
          location
        };
      })
    );

  return new Promise((resolve, reject) => {
    const dataSet = [];
    observable.subscribe({
      next: (city) => {
        dataSet.push(city);
      },
      complete: async () => {
        const bulkData = dataSet.flatMap(doc => [{ index: { _index: elSearch.indexName } }, doc]);
        const { body: bulkResponse } = await elasticSearchClient.bulk({
          refresh: true,
          body: bulkData
        });
        resolve(bulkResponse);
      },
      error: (err) => reject(err)
    });
  });
};

const createAndLoadIndex = async args => {
  const { cityDataFile, indexName } = args;
  const cityDataPath = path.resolve(cityDataFile);
  const elSearch = new SearchCityFromEs({
    indexName
  });
  const isIndexExist = await elSearch.isIndexExists();
  if (!isIndexExist) {
    await elSearch.createIndex();
    const res = loadIndex(cityDataPath, elSearch);
    return res;
  }
  return false;
};

const argv = yargs
  .usage('Usage: $0 -p [cityDataFile] -i [indexName]')
  .command('createIndex <cityDataFile> [indexName]', 'Create City Data Index', {
    cityDataFile: {
      alias: 'p',
      demandOption: true,
      type: 'string',
      description: 'City Data File Path'
    },
    indexName: {
      alias: 'i',
      demandOption: false,
      type: 'string',
      default: 'canada_us_cities'
    }
  })
  .example('$0 add ./data/cities_canada-usa.tsv')
  .help().argv;

createAndLoadIndex(argv)
  .then(() => console.log('Data Imported Success'))
  .catch(err => console.error(err));
