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
  let response = await pool.query('CREATE EXTENSION IF NOT EXISTS pg_trgm');
  response = await pool.query('CREATE EXTENSION IF NOT EXISTS cube');
  response = await pool.query('CREATE EXTENSION IF NOT EXISTS earthdistance');

  await pool.query("DROP TABLE IF EXISTS cities");
  await pool.query("CREATE TABLE cities(id SERIAL PRIMARY KEY, name VARCHAR(255), long_name VARCHAR(255), geolocation POINT, country VARCHAR(255), population INT)")
  insertData();
}

insertData = async () => {
  papa.parse(file, {
    delimiter: '\t',
    header: true,
    encoding: 'utf-8',
    complete: function(results) {
      console.log("Finished:", results);
    },
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