const childProcess = require('child_process');
const fs = require('fs');
const path = require('path');
const readline = require('readline');
const Sequelize = require('sequelize');
const { promisify } = require('util');
const { db } = require('../app/db');
const { City } = require('../app/models');

const exec = promisify(childProcess.exec);

const {
  database, host, username, password,
} = db.config;
const env = { ...process.env, PGPASSWORD: password };
const dataFile = path.resolve(__dirname, '..', 'data/cities_canada-usa.tsv');


function postgresCmd(cmd) {
  return `psql -h ${host} -U ${username} ${database} -c "${cmd}"`;
}

async function importData() {
  console.info('>>> Importing data...');

  // Ensure that PostGIS is activated
  try {
    await exec(postgresCmd('CREATE EXTENSION IF NOT EXISTS postgis;'), { env });
  } catch (err) {
    console.error('PostGIS activation failed. Error:', err.stderr);
    process.exit(1);
  }

  // Drop database table and recreate
  try {
    await City.sync({ force: true });
  } catch (err) {
    console.error('Failed to create table. Error:', err.message);
    process.exit(1);
  }

  // Grab the table headers to know what to copy in vs. the table schema
  let columns;
  try {
    columns = await new Promise((resolve, reject) => {
      const readStream = fs.createReadStream(dataFile);
      const rl = readline.createInterface({ input: readStream });
      rl.on('line', (line) => {
        rl.close();
        resolve(line.split('\t').join(','));
      });
      rl.on('error', (err) => {
        reject(err);
      });
      readStream.on('error', (err) => {
        reject(err);
      });
    });
  } catch (err) {
    console.error("Couldn't find or read data file. Error:", err.message);
    process.exit(1);
  }

  const copyCmd = postgresCmd(`\\copy cities (${columns}) FROM '${dataFile}' WITH CSV HEADER DELIMITER E'\\t' QUOTE '$'`);

  // Execute the COPY command to copy data via PostgreSQL client
  try {
    await exec(copyCmd, { env });
  } catch (err) {
    console.error('Data import failed. Error:', err.stderr);
    process.exit(1);
  }

  // Populate "geom" column using latitude/longitudes.
  try {
    console.log('>>> Creating spatial references...');
    await City.update({
      geom: Sequelize.fn('ST_SetSRID',
        Sequelize.fn('ST_Point', Sequelize.col('long'), Sequelize.col('lat')),
        4326),
    }, { where: {} });

    console.log('>>> Data import complete!');
    process.exit(0);
  } catch (err) {
    console.error('Spatial referencing failed. Error:', err.message);
    process.exit(1);
  }
}

importData();
