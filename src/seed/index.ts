import { createReadStream } from 'fs';
import { createInterface } from 'readline';

import prisma from '../db';
import { createLocation, updateIndexedLocations } from './lib';

import type { CSVLocation } from './types';

const FILE_PATH = './data/cities_canada-usa.tsv';

(async () => {
  await prisma.location.deleteMany({});

  const fileStream = createReadStream(FILE_PATH, { encoding: 'utf-8' });
  const rl = createInterface({ input: fileStream, crlfDelay: Infinity });

  let firstColumn = true;

  const data: CSVLocation[] = [];

  for await (const line of rl) {
    const columns = line.split('\t');

    // Indexing the columns below may seem slow if done in a loop, but it's a one-time operation.
    // Since we know exactly how many columns need indexing, the impact is negligible.
    // We're optimizing for performance by updating the indexed locations for the first column.
    // For subsequent columns, we add them to the data array using createLocation().
    if (firstColumn) {
      updateIndexedLocations(columns);
      firstColumn = false;
    } else {
      data.push(createLocation(columns));
    }
  }

  rl.close();
  fileStream.close();
  console.log('File parsing complete.');

  await prisma.location.createMany({ data });
  console.log('Seed complete!');
})();
