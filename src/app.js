const config = require('../config')
const http = require('http');
const querystring = require('querystring');
const query = require('./query/suggestions.query')
const suggestionResponse = require('./user/response/suggestion.response')
const errorResponse = require('./user/response/error.response')
const geoNamesImporterCsv = require('./user/importer/geoname.csv.importer')

// geoNamesImporterCsv(process.cwd() + '/data/cities_canada-usa.tsv')

module.exports = http.createServer(async function (req, res) {
    if (req.url.indexOf('/suggestions') === 0) {
        const rawQueryString = req.url.split('?').slice(-1)[0]
        const params = querystring.parse(rawQueryString)
        if (params.q === undefined) {
            return errorResponse.badRequest(res, "Querystring 'q' must be informed.")
        }
        const q = params.q
        const latitude = params.latitude || null
        const longitude = params.longitude || null
        return query.search(q, latitude, longitude)
            .then(results => suggestionResponse.success(res, results))
            .catch((reason) => errorResponse.internalError(res, reason))
    }

    res.end();
}).listen(config.port, config.hostname);

console.log('Server running at %s:%d/suggestions', config.hostname, config.port);
