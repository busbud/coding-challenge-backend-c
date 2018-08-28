const express = require('express');
const methods = require(`methods`);
const glob = require(`glob-all`);

const log = require('./logger')('routesLoader');

function asyncMiddleware(fn) {
  return function(req, res, next) {
    Promise.resolve(fn(req, res, next)).catch(e => {
      next(e);
    });
  };
};

module.exports = function loadRoutes({app, path}) {
  // Create router
  const router = express.Router();

  // Add asyncMiddleware to every route so we can use async / await in route handlers
  methods.concat(`all`).forEach(method => {
    router[method] = function(path, ...args) {
      args[args.length - 1] = asyncMiddleware(args[args.length - 1]);
      const route = router.route(path);
      route[method](...args);
      return route;
    };
  });

  // Load the routes
  const routeFiles = glob.sync([`*`, `!*.spec.js`], {cwd: path});

  for(const routeFile of routeFiles) {
    try {
      require(`${path}/${routeFile}`)(router);
    } catch(e) {
      log.f(`Error loading route "${routeFile}"`, e);
    }
  }

  // Attach router to app
  app.use(router);
  log.i(`${routeFiles.length} route file(s) loaded`);
};
