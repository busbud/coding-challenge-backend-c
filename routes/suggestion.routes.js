import express from 'express';
import suggestionController from '../components/suggestion/suggestion.controller.js';
import suggestionValidator from '../components/suggestion/suggestion.validator.js';

const router = express.Router();
router.route('').get(suggestionValidator.validateGet, suggestionController.getSuggestion);

export default router;
