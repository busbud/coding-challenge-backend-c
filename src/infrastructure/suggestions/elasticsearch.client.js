const { Client } = require('@elastic/elasticsearch');
const elasticSearchConfig = require('../../../config').elasticSearch;

const client = new Client({ node: elasticSearchConfig.baseUrl });

module.exports = {
  client,
  index: elasticSearchConfig.index,
};
