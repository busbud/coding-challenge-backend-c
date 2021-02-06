const SuggestionRouter = require('express').Router();

const SuggestionController = require('../controller/suggestionController');

/**
 * This endpoint returns suggestions for a given query and location
 */
SuggestionRouter.get('/', SuggestionController.getSuggestions);

module.exports = SuggestionRouter;
