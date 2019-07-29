/**
 * This module is used to init the elasticsearch index specified in the config
 * and upload the data contained in the tsv file. check config for more details.
 */
const tsv = new require('tsv');
const path = require('path');
const fs = require('fs');
const es = require('@elastic/elasticsearch');

const db = require('../db');
const config = require('../config').get();
const models = require('../db/models');

async function initialize() {
  console.log('Initializing db...')
  let client;

  console.log('init client...');
  try {
    client = await db.getClient(config.db, es);
  }
  catch (err) {
    console.error(err);
    process.exit(1);
  }

  console.log('init index...');
  // create index if it doesn't exist
  try {
    await db.initIndex(config.data.index, client);
  }
  catch (err) {
    console.error(err);
    process.exit(0);
  }

  console.log('put mappings...');
  // create mappings
  try {
    await db.putMapping(models.city, config.data.index, config.data.type, client);
  }
  catch (err) {
    console.error(err.body.error);
    process.exit(0);
  }

  let filePath = path.join(__dirname, '..', config.data.path);
  let file = fs.readFileSync(filePath).toString();

  // update this part. maybe use readlines again
  let bulk = tsv
    .parse(file)
    .reduce((accumulator, item) => {
      if (item.id) {
        let lat = Number(item.lat);
        let lon = Number(item.long);

        // update item fields
        item.location = {lat, lon};
        item.id = item.id.toString();
        item.admin1 = item.admin1.toString();
        item.admin2 = item.admin2.toString();
        item.admin3 = item.admin3.toString();
        item.admin4 = item.admin4.toString();
        delete item.lat;
        delete item.long;

        accumulator.push(
          { index:  { _index: config.data.index } },
          item
        );
      }

      return accumulator
  }, []);

  console.log(`pushing ${bulk.length} cities to db`, );
  try {
    let response = await db.bulkUpload(bulk, config.data.index, client);
    console.log(response);
  }
  catch (err) {
    console.error(err);
    process.exit(0);
  }

  console.log('data successfully uploaded to db');
}

initialize();