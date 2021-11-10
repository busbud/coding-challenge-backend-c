
import express from 'express';
const router = express.Router();
import AutocompleteController from '../controllers/AutocompleteController';
import RequestContract from '../types/RequestContract';
 
/** Define routes */
router.get('/suggestions', AutocompleteController.suggestions);

export default router;