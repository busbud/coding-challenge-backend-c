const express = require('express');

const { suggestionsValidator } = require('./validation');
const { suggestionsController } = require('./controllers');

exports.createServer = () => {
  const app = express();

  app.get('/suggestions', suggestionsValidator, suggestionsController.get);

  return app;
};
