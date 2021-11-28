const { Router } = require('express');
const suggestionsRouter = require('./Suggestions');

const routes = Router();

routes.use('/suggestions', suggestionsRouter);

module.exports = routes;
