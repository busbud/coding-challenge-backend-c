const elasticsearch = require('elasticsearch');

const host = process.env.ELASTICSEARCH_URL || 'localhost:9200';
const esClient = new elasticsearch.Client({ host });

module.exports = esClient;
