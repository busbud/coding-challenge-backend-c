const _ = require('lodash');
const elasticsearch = require('elasticsearch');
const fs = require('fs');
const path = require('path');
const eStream = require("event-stream");
const h = require('highland');
const { Writable } = require('stream');


class ESWritableStream extends Writable {
  constructor(esClient, esIndex) {
    super({ objectMode: true });
    this.esClient = esClient;
    this.esIndex = esIndex;
  }

  _write(body, encoding, callback) {
    console.log('Indexing %d documents', body.length / 2);
    this.esClient.bulk({
      index: this.esIndex,
      body: body
    }).then((result) => {
      console.log('Indexed %d documents', result.items.length);
      callback();
    }, (err) => {
      console.log(err, 'Error indexing documents');
      callback(err);
    });
  }
}

function importCities(esClient) {
  const filePath = path.resolve(path.join(__dirname, '../data', 'cities_canada-usa.tsv'));
  const fileStream = fs.createReadStream(filePath)
  // Split Strings
  .pipe(eStream.split("\n"))
  // Split Strings into Array
  .pipe(eStream.mapSync(function(data) {
    return data.split("\t");
  }))
  .pipe(eStream.mapSync(function(data) {
    return {
      name: data[2],
      rawName: data[2], // not analyzed in ES
      country: data[8],
      location: {
        lat: Number(data[4]),
        lon: Number(data[5])
      }
    }
  }));

  return new Promise((resolve, reject) => {
    h(fileStream)
      .flatMap((row) => {
        return [
          {index: {_index: 'cities', _type: 'city'}},
          row
        ];
      })
      .batch(2000)
      .pipe(new ESWritableStream(esClient, 'cities'))
      .on('finish', resolve)
      .on('error', err => reject(err))
  });
}

function createCitiesIndex(esClient, esConfig) {
  return Promise.resolve(esClient.indices.exists({
    index: 'cities'
  })).then((indexExists) => {
    if (indexExists) {
      return;
    } else {
      console.log('indexing cities');
      return esClient.indices.create({
        index: 'cities',
        body: esConfig.indexes.cities
      }).then(() => importCities(esClient))
        .then(() => {
          console.log('done indexing cities');
        });
    }
  });
}

function checkEsClientConnection(esClient) {
  return new Promise((resolve, reject) => {
    esClient.ping({
      // ping usually has a 3000ms timeout
      requestTimeout: 1000
    }, (error) => {
      if (error) {
        return reject(error);
      } else {
        console.log('Connected to Elasticsearch');
        return resolve(esClient);
      }
    });
  });
}

module.exports.connectToES = function(esConfig) {
  const esClient = new elasticsearch.Client(_.pick(esConfig, ['apiVersion', 'host']));
  return checkEsClientConnection(esClient)
    .then(() => createCitiesIndex(esClient, esConfig))
    .then(() => {
      return esClient;
    })
};
