const _ = require('lodash');
const elasticsearch = require('elasticsearch');

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

module.exports.connectToES= function(esOptions) {
  const esClient = new elasticsearch.Client(esOptions);
  return checkEsClientConnection(esClient)
    .then(() => {
      return esClient;
    })
    // .then(createCitiesIndex)
};
