import { Router } from 'express';
import SuggestionsController from './../controllers/SuggestionsController';
import RedirectController from './../controllers/RedirectController';

const routes = Router();
const suggestionController = new SuggestionsController();
const redirect = new RedirectController();

routes.get('/suggestions', suggestionController.handler.bind(suggestionController));
routes.get('/', redirect.handler);

export default routes;
