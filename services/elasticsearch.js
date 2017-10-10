const elasticsearch = require('elasticsearch');

const host = process.env.ELASTICSEARCH_URL || 'localhost:9200';

module.exports = () => new elasticsearch.Client({ host });
