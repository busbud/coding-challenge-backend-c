'use strict';

const logger = require(_src + '/libs/logger');
const _ = require('lodash');
const constants = require(_src + '/config/constants');
const swaggerDoc = require(_src + '/api/search.json');
const swaggerTools = require('swagger-tools');
const API_ROOT = '/';
const swaggerOpts = {
  controllers: _src + '/dispatchers', // This is an absolute path
  useStubs: process.env.NODE_ENV === 'development' // Not used
};
const errorCodes = require(_src + '/config/errors').errorCodes;

module.exports = function(app) {
  // Initialize the Swagger middleware
  swaggerTools.initializeMiddleware(swaggerDoc, function(middleware) {
    // Interpret Swagger resources and attach metadata to request - must be first in swagger-tools middleware chain
    app.use(API_ROOT, middleware.swaggerMetadata());

    // Serve the Swagger documents and Swagger UI
    // Can be accessed through /docs and /api-docs
    // app.use(API_ROOT, middleware.swaggerUi());
    if (process.env.SHOW_DOC) {
      app.use(middleware.swaggerUi({
        apiDocs: '/api-docs',
        swaggerUi: '/docs'
      }));
    }
    // Provide the security handlers
    app.use(middleware.swaggerSecurity({}));

    // Validate Swagger requests
    app.use(middleware.swaggerValidator({
      validateResponse: false
    }));

    // Route validated requests to appropriate controller
    app.use(API_ROOT, middleware.swaggerRouter(swaggerOpts));

    // Error handler
    app.use(function(err, req, res, next) { // eslint-disable-line no-unused-vars
      logger.error('Swagger ERROR : %s', JSON.stringify(err));
      // set status to Failed
      const errorResponse = {
        status: constants.status.FAILED
      };
      if (!_.isObject(err)) {
        // If the object is not an Error, create a representation that appears to be
        err = {
          message: String(err) // Coerce to string
        };
        errorResponse.message = err;
      } else {
        // err.message is not enumerable by default
        Object.defineProperty(err, 'message', {
          enumerable: true
        });
        if (process.env.DEV_MODE) {
          errorResponse.error = {
            code: 999,
            details: _.get(err, 'results.errors'),
            message: _.get(err, 'message'),
            type: _.get(err, 'code')
          };
        } else {
          errorResponse.error = {
            code: errorCodes.BAD_PARAMETER_FORMAT.code,
            details: 'Request Malformed',
            message: errorCodes.BAD_PARAMETER_FORMAT.errorMessage
          };
        }
      }
      logger.error(errorResponse.message);
      // Return a JSON representation of #/definitions/ErrorResponse
      res.setHeader('Content-Type', 'application/json');
      res.end(JSON.stringify(errorResponse));
    });
  });
};
