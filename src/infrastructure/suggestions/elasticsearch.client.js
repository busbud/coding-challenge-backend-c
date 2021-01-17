const elasticSearchConfig = require('../../../config').elasticSearch
const {Client} = require('@elastic/elasticsearch')
console.log(elasticSearchConfig)
const client = new Client({node: elasticSearchConfig.baseUrl})

module.exports = {
    client: client,
    index: elasticSearchConfig.index
}
