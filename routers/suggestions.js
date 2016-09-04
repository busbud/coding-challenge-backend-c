var express     = require('express');
var controller  = require('../controllers/suggestions');

var suggestions = express();

suggestions.get('/', controller.search);

module.exports = suggestions;