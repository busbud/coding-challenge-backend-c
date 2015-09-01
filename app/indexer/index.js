import CityIndexer from './cityindexer';
import fs from 'fs';
import csv from 'csv';
import CitiesSchema from '../schema/cities';
import client from '../elastic.js';

const indexer = new CityIndexer(client);

const parser = csv.parse({
  delimiter: '\t',
  quote: '""""',
  escape: '""""',
});

const schema = new CitiesSchema(client);

console.log('Load schema and index mapping');
schema.load().then(() => {
  console.log('import data');
  const readStream = fs.createReadStream('data/cities_canada-usa.tsv', {
    encoding: 'utf-8'
  });

  readStream.pipe(parser);

  parser.on('readable', (record) => {
    var record;
    while (record = parser.read()) {
      indexer.index(record[0], {
        name: record[2],
        alt_name: record[3],
        population: record[14],
        country: record[8],
        admin_1: record[10],
        location : {
          lat: record[4],
          lon: record[5]
        }
      });
    }
  });

  parser.on('finish', () => {
    indexer.flushAll().then(() => {
      client.close();
      console.log('DONE');
      process.exit(0);
    }, () => {
      client.close();
      process.exit(1);
    });
  });
});
