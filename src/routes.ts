import SuggestionsController from "./controllers/SuggestionsController";

const routes = require('express').Router();

routes.get('/suggestions', new SuggestionsController().handler)

export default routes;
