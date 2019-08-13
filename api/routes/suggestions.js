const express = require('express');
const router = express.Router();
const SuggestionsController = require('../controllers/suggestions');

router.get('/',
  SuggestionsController.validate(),
  SuggestionsController.cities_get_filtered);

module.exports = router;
