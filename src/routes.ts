import SuggestionsController from './controllers/SuggestionsController';
import { Router } from 'express';

const routes = Router();
const suggestionController = new SuggestionsController();

routes.get('/suggestions', suggestionController.handler.bind(suggestionController));

export default routes;
