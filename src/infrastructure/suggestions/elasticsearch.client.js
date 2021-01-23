const { Client } = require('@elastic/elasticsearch');
const elasticSearchConfig = require('../../../config').elasticSearch;

const options = {
  node: elasticSearchConfig.baseUrl,
};
if ('agent' in elasticSearchConfig) {
  options.agent = elasticSearchConfig.agent;
}

const client = new Client(options);

module.exports = {
  client,
  index: elasticSearchConfig.index,
};
