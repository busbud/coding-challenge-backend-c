module.exports = (app) => {
  const controller = require('../controllers/healthCheckController');
  const router = require('express').Router();

  router.get('/healthCheck', controller.checkHealth);

  app.use(router);
};
