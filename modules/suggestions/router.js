var express     = require('express');
var suggestions = express();
var controller  = require('./controller');

//console.log(new controller())

suggestions.get('/', controller.get)

module.exports = suggestions;