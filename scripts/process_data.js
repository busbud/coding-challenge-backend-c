if (process.env.NODE_ENV !== 'production') {
  require('dotenv').config();
}
const fs = require('fs');
const pg = require('pg');
const papa = require('papaparse');
const dbConfig = require('../config/db');
const filePath = __dirname + '/../data/cities_canada-usa.tsv';
const file = fs.createReadStream(filePath);

const Pool = pg.Pool;

const pool = new Pool(dbConfig);


const province_code_mapping = ['', 'AL', 'BC', 'NB', 'NL', '', 'NS', 'ON', 'PE', 'QC', 'SK', 'YK', 'NT', 'NU'];
const country_code_mapping= [];
country_code_mapping['CA']= ['Canada'];
country_code_mapping['US'] = ['USA'];

initDataBase = async () => {
  console.log('Creating extension pg_trgm...');
  await pool.query('CREATE EXTENSION IF NOT EXISTS pg_trgm');
  console.log('Creating extension cube...');
  await pool.query('CREATE EXTENSION IF NOT EXISTS cube');
  console.log('Creating extension earthdistance...');
  await pool.query('CREATE EXTENSION IF NOT EXISTS earthdistance');

  console.log('Creating table cities...');
  await pool.query("DROP TABLE IF EXISTS cities");
  await pool.query("CREATE TABLE cities(id SERIAL PRIMARY KEY, name VARCHAR(255), long_name VARCHAR(255), geolocation POINT, country VARCHAR(255), population INT)")
  await pool.query("CREATE INDEX index_city_name ON cities USING btree (name)")
  console.log('inserting data...');
  insertData();
}

insertData = async () => {
  papa.parse(file, {
    delimiter: '\t',
    header: true,
    encoding: 'utf-8',
    step: function(results, parser) {
      const data = results.data;
      const country = data.country;
      const cityName = data.name.replaceAll("'", "''");
      const population = data.population;
      let longCityName = '';
      if(country === 'CA') {
        longCityName = `${cityName}, ${province_code_mapping[Number(data.admin1)]}, ${country_code_mapping[data.country]}`
      } else {
        longCityName = `${cityName}, ${data.admin1}, ${country_code_mapping[data.country]}`
      }
      pool.query(`INSERT INTO cities(name, long_name, geolocation, country, population) values('${cityName}', '${longCityName}', point(${data.long}, ${data.lat}), '${country}', '${population}')`);
    }
  })
}


initDataBase();