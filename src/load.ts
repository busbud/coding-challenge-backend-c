import { connect, Connection } from 'api/db';
import * as config from 'api/config';
import readLine from 'readline';
import fs from 'fs';
import { City } from 'api/schema';

async function run() {
  await connect()
    .then(connection => load(connection))
    .catch(e => console.error(`Unable to execute data load: ${e}`));
}

// Loads the data specified at the configured path into the Redis data store.
export async function load(connection: Connection) {
  // Clear out any existing data in Redis.
  await connection.flushdb();
  
  // Read the data file one line at a time, process, and set appropriate hashes/sets in Redis.
  const stream = fs.createReadStream(config.DATA_FILE_PATH);
  const lineReader = readLine.createInterface({
    input: stream,
    crlfDelay: Infinity
  });

  let recordCount = 0;
  let loadCount = 0;
  console.info("Loading city data. Please wait...");
  for await (const line of lineReader) {
    // Skip first line
    if (recordCount == 0) {
      recordCount++;
      continue;
    }
    let fields = line.split('\t');

    // Do not process cities not within the configured population threshold or that are outside of CA/USA.
    const pop = parseInt(fields[config.DATA_POPULATION_INDEX] || '0'); 
    if (pop < config.DATA_CITY_MIN_POP || !['CA', 'US'].includes(fields[config.DATA_COUNTRY_INDEX]))
      continue;
    else
      loadCount++;
      
    // Create a record for each line containing relevant data.
    const city: City = {
     id: fields[config.DATA_ID_INDEX],
     name: fields[config.DATA_NAME_INDEX],
     asciiName: fields[config.DATA_ASCII_NAME_INDEX],
     territory: fields[config.DATA_TERR_INDEX],
     country: fields[config.DATA_COUNTRY_INDEX],
     latitude: parseFloat(fields[config.DATA_LAT_INDEX]),
     longitude: parseFloat(fields[config.DATA_LONG_INDEX])
    };
    await connection.hset('city-hash', city.id, JSON.stringify(city)).catch(e => console.error(e));
    recordCount++;

    // Set up city name prefixes for name lookup
    const namePrefixes = createNamePrefixes(city.asciiName);
    for (const prefix of namePrefixes) {
      let score = (1 - ((city.asciiName.length - prefix.length) / city.asciiName.length)).toFixed(1);
      await connection.zadd(`city-name-index:${prefix}`, score, city.id).catch(e => console.error(e));
    }
  }
  console.info(`Loaded data for ${loadCount} cities.`);
  process.exit();
}

function createNamePrefixes(field: string): string[] {
  const words = field.split(' ');
  const prefixes = [];
  for (const word of words) {
    for (let i = 1; i < word.length; i++) {
      prefixes.push(word.substring(0, i+1));
    }
  }
  return prefixes;
}

run();
