const config = require('../config')
const http = require('http');
const router = require('./user/rest/router')

module.exports = http.createServer(router).listen(config.port, config.hostname);
console.log('Server running at %s:%d/suggestions', config.hostname, config.port);
