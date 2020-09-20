import express from 'express';
import suggestionController from '../components/suggestion/suggestion.controller.js';

const router = express.Router();
router.route('').get(suggestionController.getSuggestion);

export default router;
