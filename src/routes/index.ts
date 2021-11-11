
import express from 'express';
const router = express.Router();
import AutocompleteController from '../controllers/AutocompleteController';
 
/** Define routes */
router.get('/suggestions', AutocompleteController.suggestions);

export default router;