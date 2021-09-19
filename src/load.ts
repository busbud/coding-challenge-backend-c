import { Connection } from 'api/db';
import { DATA_FILE_PATH, DATA_ID_INDEX, DATA_NAME_INDEX, DATA_ASCII_NAME_INDEX, DATA_LAT_INDEX, DATA_LONG_INDEX } from 'api/config';
import readLine from 'readline';
import fs from 'fs';
import { City } from 'api/schema';
import { connect } from 'api/db';

async function run() {
  connect()
    .then(connection => load(connection))
    .catch(e => console.error(`Unable to execute data load: ${e}`));
}

// Loads the data specified at the configured path into the Redis data store.
async function load(connection: Connection) {
  // Clear out any existing data in Redis.
  await connection.flushdb();
  
  // Read the data file one line at a time, process, and set appropriate hashes/sets in Redis.
  const stream = fs.createReadStream(DATA_FILE_PATH);
  const lineReader = readLine.createInterface({
    input: stream,
    crlfDelay: Infinity
  });

  let recordCount = 0;
  console.info("Loading city data. Please wait...");
  for await (const line of lineReader) {
    // Skip first line
    if (recordCount == 0) {
      recordCount++;
      continue;
    }

    // Create a record for each line containing relevant data.
    let fields = line.split('\t');
    const city: City = {
     id: fields[DATA_ID_INDEX],
     name: fields[DATA_NAME_INDEX],
     asciiName: fields[DATA_ASCII_NAME_INDEX],
     latitude: parseFloat(fields[DATA_LAT_INDEX]),
     longitude: parseFloat(fields[DATA_LONG_INDEX])
    };
    await connection.hset('city-hash', city.id, JSON.stringify(city)).catch(e => console.error(e));
    recordCount++;

    // Set up city name prefixes for name lookup
    const namePrefixes = createNamePrefixes(city.asciiName);
    for (const prefix of namePrefixes) {
      await connection.zadd(`city-name-index:${prefix}`, 0, city.id).catch(e => console.error(e));
    }

    // Set up lat/long prefixes for location lookup
    await connection.geoadd('city-geo-index', city.longitude, city.latitude, city.id).catch(e => console.error(e));
  }
  console.info(`Loaded data for ${recordCount} cities.`);
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
