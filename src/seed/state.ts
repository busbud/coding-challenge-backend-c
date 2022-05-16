import fs from 'fs';
import { Knex } from 'knex';
import { parse } from 'csv-parse';

export const seed = async (db: Knex) => {
  await db('state').truncate();

  const stream = fs.createReadStream('data/states_canada-usa.tsv');

  const parser = stream.pipe(parse({
    from_line: 2,
    delimiter: '\t',
    quote: false,
    columns: ['country', 'admin1', 'name', false, 'id'],
    cast: (value, context) => {
      if (
        !context.header
        && ['id'].includes(String(context.column))
      ) {
        return Number(value);
      }
      return value;
    },
  }));

  for await (const record of parser) {
    await db('state').insert(record);
  }

  stream.close();
};
