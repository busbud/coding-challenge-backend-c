/* eslint-disable import/no-extraneous-dependencies */
/* eslint-disable quote-props */

/**
 * Script responsible to seed the cities index.
 * All methods in here are intentionally city-specific.
*/

import fs from 'fs';
import { parse } from 'csv-parse';
import { IndicesCreateRequest } from '@elastic/elasticsearch/api/types';
import { ElasticSearchClient } from '../src/clients/elasticsearch-client';
import { City } from '../src/services/suggestion-service';

interface TsvCity {
  id: number;
  name: string;
  ascii: string;
  alt_name: string;
  lat: number;
  long: number;
  feat_class: string;
  feat_code: string;
  country: string;
  cc2: string;
  admin1: string;
  admin2: string;
  admin3: string;
  admin4: string;
  population: number;
  elevation: number;
  dem: number;
  timezone: string;
  modified_at: Date;
}

const canadaProvinceMap = {
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
  '12': 'NT',
  '13': 'NU',
  '14': 'YT',
};

const createCitiesIndex = async (client: ElasticSearchClient) => {
  const options: IndicesCreateRequest = {
    index: 'cities',
    body: {
      settings: {
        analysis: {
          analyzer: {
            standard_asciifolding: {
              tokenizer: 'standard',
              filter: ['lowercase', 'asciifolding'],
            },
          },
        },
      },
      mappings: {
        properties: {
          geoname_id: { type: 'integer' },
          name: { type: 'text' },
          name_concat: {
            type: 'search_as_you_type',
            analyzer: 'standard_asciifolding',
            search_analyzer: 'standard_asciifolding',
          },
          province: { type: 'text' },
          country: { type: 'text' },
          location: { type: 'geo_point' },
        },
      },
    },
  };

  const statusCode = await client.createIndex(options);

  return statusCode;
};

const processTsvCities = async (): Promise<TsvCity[]> => {
  const records: TsvCity[] = [];
  const parser = fs
    .createReadStream(`${__dirname}/data/cities_canada-usa.tsv`)
    .pipe(parse({ delimiter: '\t', quote: false, columns: true }));

  for await (const record of parser) {
    if (record.population >= 5000) records.push(record);
  }

  return records;
};

const transformCities = (tsvCities: TsvCity[]) => {
  const cities: City[] = tsvCities.map((tsvCity) => {
    const province = tsvCity.country === 'CA'
      ? canadaProvinceMap[tsvCity.admin1 as keyof typeof canadaProvinceMap] : tsvCity.admin1;

    const nameConcat = `${tsvCity.name}, ${province} ${tsvCity.country}`;

    return {
      geoname_id: Number(tsvCity.id),
      name: tsvCity.name,
      name_concat: nameConcat,
      province,
      country: tsvCity.country,
      location: { lat: Number(tsvCity.lat), lon: Number(tsvCity.long) },
      population: Number(tsvCity.population),
    };
  });

  return cities;
};

(async () => {
  try {
    const client = new ElasticSearchClient({node: 'http://localhost:9200'});
    const citiesIndex = await client.isExistentIndex('cities');

    if (!citiesIndex) createCitiesIndex(client);

    const tsvCities = await processTsvCities();
    const cities = transformCities(tsvCities);
    const result = await client.bulkInsert('cities', cities, 'geoname_id');

    console.log('Cities successfully loaded. Results:', result);
  } catch (error) {
    console.log('Uh, something went wrong', JSON.stringify(error));
  }
})();
