const _ = require('lodash');
const elasticsearch = require('elasticsearch');
const fs = require('fs');

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

function createCitiesIndex(esClient, esConfig) {
  return Promise.resolve(esClient.indices.exists({
    index: 'cities'
  })).then((indexExists) => {
    console.log(indexExists);
    if (indexExists) {
      console.log('cities already exists');
      return;
    } else {
      console.log('indexing cities');
      return Promise.resolve(esClient.indices.create({
        index: 'cities',
        body: esConfig.indexes.cities
      }))
        // .then(() => importCities(esClient).then(() => {
        // console.log('done indexing cities');
      // }));
    }
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
