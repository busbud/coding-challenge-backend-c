const suggestionCommand = require('../../../command/suggestions.command')
const maintenanceResponse = require('../response/maintenance.response')
const elasticsearchIndex = require('../../../infrastructure/suggestions/elasticsearch.index')
const handler = async (req, res) => {
    if (req.url.indexOf('/maintenance/populate') === 0 && req.method === 'POST') {
        suggestionCommand.importFile(process.cwd() + '/data/cities_canada-usa.tsv')
        maintenanceResponse.response(res, 'executing', 202);
        return true
    }

    if (req.url.indexOf('/maintenance/index') === 0 && req.method === 'DELETE') {
        await elasticsearchIndex.deleteIndex()
        maintenanceResponse.response(res, 'deleted', 200);
        return true
    }

    if (req.url.indexOf('/maintenance/index') === 0 && req.method === 'POST') {
        await elasticsearchIndex.createIndex(process.cwd() + '/suggestions_index.json')
        maintenanceResponse.response(res, 'created', 201);
        return true
    }
    return false
}

module.exports = handler
