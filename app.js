#!/usr/bin/env node

/**
 * Busbud App
 * 
 * @author Mohammad Fares <faressoft.com@gmail.com>
 */

/**
 * All app's dependencies should be included here
 * and only here, so we can know what we have exactly
 */

// Third-part modules
var express          = require('express'),
    path             = require('path'),
    RoutePattern     = require('route-pattern'),
    http             = require('http'),
    parseFunction    = require('parse-function'),
    bodyParser       = require('body-parser'),
    moment           = require('moment'),
    morgan           = require('morgan'),
    slashes          = require('connect-slashes'),
    serializeError   = require('serialize-error'),
    responseTime     = require('response-time'),
    changeCase       = require('change-case'),
    is               = require('is_js'),
    q                = require('q'),
    async            = require('async'),
    fs               = require('fs'),
    merge            = require('merge'),
    deepmerge        = require('deepmerge'),
    deepcopy         = require('deepcopy'),
    lodash           = require('lodash'),
    geolib           = require('geolib'),
    glob             = require('glob'),
    commentParser    = require('comment-parser'),
    chalk            = require('chalk');

// Core modules
var Router           = require('./router.js'),
    Response         = require('./response.js'),
    server           = require('./server.js'),
    config           = require('./config.js'),
    tasks            = require('./tasks.js'),
    di               = require('./di.js'),

// Helpers and misc modules
    validate         = require('./app/helpers/validate.js'),
    text             = require('./app/helpers/text.js'),
    city             = require('./app/helpers/city.js'),
    Trie             = require('./app/helpers/trie.js'),
    Client           = require('./app/helpers/client.js'),
    objectChangeCase = require('./app/helpers/object_change_case.js'),
    validators       = require('./app/misc/validators.js'),
    normalizers      = require('./app/misc/normalizers.js');

var app              = express(),
    router           = null;

// Define the DI as a global object
global.di = di;

// Server express app over a proxy
app.set('trust proxy', 1);

// Dependency Injection
di.set('config', config);
di.set('http', http);
di.set('text', text);
di.set('city', city);
di.set('Trie', Trie);
di.set('validate', validate);
di.set('async', async);
di.set('fs', fs);
di.set('path', path);
di.set('commentParser', commentParser);
di.set('chalk', chalk);
di.set('tasks', tasks);
di.set('moment', moment);
di.set('morgan', morgan);
di.set('slashes', slashes);
di.set('serializeError', serializeError);
di.set('responseTime', responseTime);
di.set('parseFunction', parseFunction);
di.set('bodyParser', bodyParser);
di.set('is', is);
di.set('normalizers', normalizers);
di.set('changeCase', changeCase);
di.set('objectChangeCase', objectChangeCase);
di.set('merge', merge);
di.set('deepmerge', deepmerge);
di.set('lodash', lodash);
di.set('geolib', geolib);
di.set('glob', glob);
di.set('routePattern', RoutePattern.fromString);
di.set('deepcopy', deepcopy);
di.set('Response', Response);
di.set('q', q);

// Load and cache the configurations
config.sycnLoad();

// Load custom validators
validators.sycnLoad();

// Create a router
router = new Router(app);

// Inject the router into the DI
di.set('router', router);

////////////////////////////////////////////////////
// Middlewarews ////////////////////////////////////
////////////////////////////////////////////////////

// Only for the development and staging enviroments
if (process.env.NODE_ENV !== 'production') {

  // Before routing, the controller is not known
  router.middleware.before(['cors']);
  
}

// Skip the logger middleware for the testing enviroment
if (process.env.NODE_ENV !== 'testing') {

  // Before routing, the controller is not known
  router.middleware.before(['logger']);
  
}

// Before routing, the controller is not known
router.middleware.before([
  'body_parser',
  'slashes',
  'response_time'
]);

// Before executing the controller (the controller is known)
router.middleware.controller([
  'join_params'
]);

// Before executing the service (the service is known)
router.middleware.service([
  'join_params',
  'validate',
  'set_default_values',
  'schema_params',
  'alias_params',
  'camel_case_params'
]);

// After executing the service (before sending the data)
router.middleware.after([]);

// Handling for any unhandled errors that throws from any route
router.middleware.fail([
  'not_found',
  'error_handling'
]);

////////////////////////////////////////////////////
// Load Modules And Server /////////////////////////
////////////////////////////////////////////////////

// Initialize the client
di.set('client', new Client(router));

// Create the http server and handle listening and error events
var appServer = server(app);

/**
 * Inittialization, each init module should provide a
 * method called load() that returns a promise
 */
module.exports = q.all([

  // A wrapper around the texts resource to store error messages
  text.load(),

  // Load, parse, and cache the cities names
  city.load()

]).catch(function(error) {

  console.error('[X]', error.message);

}).then(function() {

  // Mount routes and middlewares
  router.mount();

  // Start listening on the server's port and receiving requests
  appServer.start();

  // To delay the next `then` after executing the current function
  return Promise.resolve(appServer);

});
