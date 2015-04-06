var port = process.env.PORT || 2345;

var express = require('express');
var app = express();
var lessMiddleware = require('less-middleware');
var bodyParser = require('body-parser');
var Trie = require('data-structures').Trie;
var _ = require('underscore');
var expressWinston = require('express-winston');
var logger = require('./logger').logger;
var favicon = require('serve-favicon');

app.use(favicon(__dirname + '/public/favicon.ico'));

app.use(expressWinston.logger({
    transports: require('./logger').transports
}));

logger.info('reading city_info');
var city_info = require('./data/city_info.json');

logger.info('constructing Trie');
var city_trie = new Trie(_.keys(city_info));

app.use(lessMiddleware(__dirname + '/public'));

app.use(bodyParser.json());

// ROUTES
require('./api/suggestions_api')(app, city_trie, city_info);

app.use(express.static(__dirname + '/public'));

app.listen(port, '0.0.0.0');

logger.info('Server running at http://127.0.0.1:%d/suggestions', port);

module.exports = {
    app: app
};