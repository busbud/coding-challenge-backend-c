module.exports = (app) => {
  const controller = require('../controllers/searchController');
  const router = require('express').Router();

  router.get('/suggestions', controller.search);

  app.use(router);
};
