const fs = require('fs');
const csv = require('fast-csv');
const mongoose = require('mongoose');
require('dotenv').config();

const {printDate} = require('./helpers/logger');
const City = require('./models/City');

const db = process.env.MONGO_URI;

const databaseConfiguration = {
  useNewUrlParser: true,
  useUnifiedTopology: true,
  useCreateIndex: true,
  autoIndex: true,
};

mongoose
    .connect(db, databaseConfiguration)
    .then(() => connected())
    .catch((error) => {
      console.error(`${printDate()}  An error occurred while trying to connect to database: ${error}`);
      process.exit(1);
    });

const connected = () => {
  console.log(`${printDate()}  Successful connection to MongoDB.`);
  const cities = [];
  const stream = fs.createReadStream('./data/cities_canada-usa.tsv');
  const parserConfiguration = {
    delimiter: '\t',
    headers: true,
    quote: false,
    escape: false,
    relax: true,
  };
  csv
      .parseStream(stream, parserConfiguration)
      .on('error', (error) => {
        console.error(`${printDate()}  An error occurred while parsing the data file: ${error}`);
        process.exit(1);
      })
      .on('data', (data) => {
        populate(cities, data);
      })
      .on('end', () => {
        console.log(`${printDate()}  Finished parsing the data file.`);
        insert(cities);
      });
};

const populate = (array, data) => {
  array.push({
    name: data.name,
    location: {
      type: 'Point',
      coordinates: [parseFloat(data.long), parseFloat(data.lat)],
    },
    country: data.country,
    region: data.admin1,
    population: parseInt(data.population),
    timezone: data.tz,
    modified: data.modified_at,
  });
};

const insert = (data) => {
  City.collection.insertMany(data, (error) => {
    if (error) {
      console.error(`${printDate()}  An error occurred while inserting parsed data to database: ${error}`);
      process.exit(1);
    } else {
      console.log(`${printDate()}  Successful insert into the database.`);
      process.exit();
    }
  });
};
