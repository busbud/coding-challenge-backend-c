const suggestionCommand = require('../../../command/suggestions.command')
const maintenanceResponse = require('../response/maintenance.response')
const handler = async (req, res) => {
    if (req.url.indexOf('/maintenance/populate') === 0 && req.method === 'POST') {
        suggestionCommand.importFile(process.cwd() + '/data/cities_canada-usa.tsv')
        maintenanceResponse.response(res, 'executing', 202);
        return true
    }
    return false
}

module.exports = handler
