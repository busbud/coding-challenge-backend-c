const config = require('../config')
const http = require('http');
const querystring = require('querystring');
const query = require('./query/suggestions.query')
const suggestionResponse = require('./user/suggestion.response')
const errorResponse = require('./user/error.response')

module.exports = http.createServer(async function (req, res) {
    if (req.url.indexOf('/suggestions') === 0) {
        const rawQueryString = req.url.split('?').slice(-1)[0]
        const params = querystring.parse(rawQueryString)
        if (params.term === undefined) {
            return errorResponse.badRequest(res, "Querystring 'term' must be informed.")
        }
        const term = params.term
        const lat = params.lat || null
        const lon = params.lon || null
        return query.search(term, lat, lon)
            .then(results => suggestionResponse.success(res, results))
            .catch((reason) => errorResponse.internalError(res, reason))
    }

    res.end();
}).listen(config.port, config.hostname);

console.log('Server running at %s:%d/suggestions', config.hostname, config.port);
