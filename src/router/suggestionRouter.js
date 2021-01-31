const SuggestionRouter = require('express').Router();

const SuggestionController = require('../controller/suggestionController');

/**
 * This endpoint returns all events for a given device
 */
SuggestionRouter.get('/', SuggestionController.getSuggestions);

module.exports = SuggestionRouter;
