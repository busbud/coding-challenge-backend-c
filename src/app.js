const express = require('express');

const { suggestionValidator } = require('./validation');
const { suggestionController } = require('./controller');

exports.createServer = () => {
  const app = express();

  app.get('/suggestions', suggestionValidator, suggestionController.get);

  return app;
};
