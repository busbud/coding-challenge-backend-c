var express     = require('express');
var controller  = require('./controller');

var suggestions = express();

suggestions.get('/', controller.get);

module.exports = suggestions;