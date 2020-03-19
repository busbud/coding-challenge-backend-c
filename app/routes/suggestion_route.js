var express = require('express');
var suggestionController = require('../controllers/suggestion_controller');

var router = express.Router();

router.get('/', suggestionController.root);

module.exports = router;
