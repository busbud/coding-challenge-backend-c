var express     = require('express');
var controller  = require('../controllers/default');

var suggestions = express();

suggestions.get('/', controller.index);

module.exports = suggestions;