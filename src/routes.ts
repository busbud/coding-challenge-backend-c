import SuggestionsController from "./controllers/SuggestionsController";

const routes = require('express').Router();
const suggestionController = new SuggestionsController();

routes.get('/suggestions', suggestionController.handler.bind(suggestionController));

export default routes;
