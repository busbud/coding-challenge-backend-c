import fs from 'fs';
import { Knex } from 'knex';
import { parse } from 'csv-parse';

export const seed = async (db: Knex) => {
  await db('city').truncate();

  const stream = fs.createReadStream('data/cities_canada-usa.tsv');

  const parser = stream.pipe(parse({
    from_line: 2,
    delimiter: '\t',
    quote: false,
    columns: ['id', 'name', 'ascii', false, 'lat', 'long', false, false, 'country',
      false, 'admin1', false, false, false, 'population', false, false, false, false],
    cast: (value, context) => {
      if (
        !context.header
        && ['id', 'lat', 'long', 'population'].includes(String(context.column))
      ) {
        return Number(value);
      }
      return value;
    },
  }));

  for await (const record of parser) {
    await db('city').insert(record);
  }

  stream.close();
};
