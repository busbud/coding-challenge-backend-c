const config = require('../config')
const http = require('http');
const querystring = require('querystring');
const query = require('./query/suggestions.query')
const suggestionResponse = require('./user/rest/response/suggestion.response')
const errorResponse = require('./user/rest/response/error.response')
const command = require('./command/suggestions.command')
const router = require('./user/rest/router')

command.importFile(process.cwd() + '/data/cities_canada-usa.tsv')


module.exports = http.createServer(router).listen(config.port, config.hostname);
console.log('Server running at %s:%d/suggestions', config.hostname, config.port);
