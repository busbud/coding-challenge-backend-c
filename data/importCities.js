import fs from "fs";
import stream, { Transform } from "stream";
import csv from "csv-streamify";
import streamToMongo from "stream-to-mongo";
import JSONStream from "JSONStream";
import _ from "lodash";
import mongodb, { MongoClient } from "mongodb";
const jsonToStrings = JSONStream.stringify(false);
import es6promise from "es6-promise";

es6promise.polyfill();

/* There are 13 provinces and territories. For our purposes,
 * we'll just use a simple translation table. */
const provinceCodes = {
  '01': 'AB',
  '02': 'BC',
  '03': 'MB',
  '04': 'NB',
  '05': 'NL',
  '07': 'NS',
  '13': 'NT',
  '14': 'NU',
  '08': 'ON',
  '09': 'PE',
  '10': 'QC',
  '11': 'SK',
  '12': 'YT'
};

const options = {
  db: "mongodb://localhost:27017/busbud-challenge",
  collection: "cities"
};

/* Parse the TSV data into JS arrays */
const parser = csv({ objectMode: true, delimiter: '\t' });

/* Transform each parsed array into a location object for insertion into
 * mongodb. */
const transform = Transform({objectMode: true});
transform._transform = function (data, encoding, done) {

  const name = data[2];
  const country = data[8];
  const state = (country === "CA") ?
    provinceCodes[data[10]] :
    data[10];

  const plain = `${name} ${state} ${country}`.toLowerCase();
  const population = data[14];

  const location = {
    id: data[0],
    fullName: plain,
    city: data[1],
    state: state,
    country: country,
    location: {
      type: "Point",
      coordinates: [
        parseFloat(data[4]), // lat
        parseFloat(data[5])  // long
      ]
    },
    population: parseInt(population)
  };

  if (population >= 5000) {
    this.push(location);
  }

  done();
};

/* First connect to MongoDB and create an index on our collection. */
const promise = new Promise(function (resolve, reject) {
  MongoClient.connect(options.db, function (err, db) {
    if (err) {
      console.log('Unable to connect to the mongoDB server. Error:', err);
      reject("Unable to connect to the MongoDB server.");
    } else {
      console.log('Connection established to', options.db);

      db.collection("cities").createIndex({ fullName: 1 });

      //Close connection
      db.close();
      resolve()
    }
  });
});

/* Parse the data file and stream the results directly into MongoDB.
 * Un-commenting the two commented lines, and commenting out the streamToMongo
 * line, will dump the results into a .json file instead. */
promise.then(
  fs.createReadStream("./data/cities_canada-usa.tsv")
    .pipe(parser)
    .pipe(transform)
    //.pipe(jsonToStrings)
    //.pipe(fs.createWriteStream("cities.json"));
    .pipe(streamToMongo(options))
    .on('error', function(e) { console.log(e); })
);

