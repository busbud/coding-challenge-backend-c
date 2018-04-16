/**
 * Mount and handle routes
 * 
 * @author Mohammad Fares <faressoft.com@gmail.com>
 */

/**
 * Mount and handle routes
 * 
 * @param {Object} app express app
 */
module.exports = function(app) {

  var self = this;

  /**
   * Registered middlewares
   * 
   * @type {Object}
   */
  var middlewares = {
    // Before routing, the controller is not known
    before: [],
    // Before executing the controller, the controller is known
    controller: [],
    // Before executing the service, the service is known
    service: [],
    // After executing the service (before sending the data)
    after: [],
    // If an error is throws or route not found
    fail: []
  };

  /**
   * Manage middlewares
   * 
   * @type {Object}
   */
  self.middleware = {};

  /**
   * Controllers
   * @type {Array} {schema({name, actions: []})}
   */
  self.controllers = [];

  /**
   * Load a middleware module
   * 
   * @param {String} name the file name of the middleware without the extension
   */
  var loadMiddleware = function(name) {

    var middlewarePath = di.path.join(__dirname, 'app/middlewares/' + name + '.js');

    // Check if the file is exists
    try {
      var stat = di.fs.statSync(middlewarePath);
    } catch (e) {
      console.error('[X] Faild to load the middleware', name, 'it is not found');
    }

    return require(middlewarePath);

  };

  /**
   * Load and add a middleware, to be executed:
   * Before routing, the controller is not known
   * 
   * @param  {String} names the files names of the middlewares
   */
  self.middleware.before = function(names) {
    for (var i in names) {
      middlewares.before.push(loadMiddleware(names[i]));
    }
  };

  /**
   * Load and add a middleware, to be executed:
   * After executing the service (before sending the data)
   * 
   * @param  {String} names the files names of the middlewares
   */
  self.middleware.after = function(names) {
    for (var i in names) {
      middlewares.after.push(loadMiddleware(names[i]));
    }
  };

  /**
   * Load and add a middleware, to be executed:
   * Before executing the controller, the controller is known
   * 
   * @param  {String} names the files names of the middlewares
   */
  self.middleware.controller = function(names) {
    for (var i in names) {
      middlewares.controller.push(loadMiddleware(names[i]));
    }
  };

  /**
   * Load and add a middleware, to be executed:
   * Before executing the service, the service is known
   * 
   * @param  {String} names the files names of the middlewares
   */
  self.middleware.service = function(names) {
    for (var i in names) {
      middlewares.service.push(loadMiddleware(names[i]));
    }
  };

  /**
   * Load and add a middleware, to be executed:
   * If an error is throws or route not found
   * 
   * @param  {String} names the files names of the middlewares
   */
  self.middleware.fail = function(names) {
    for (var i in names) {
      middlewares.fail.push(loadMiddleware(names[i]));
    }
  };

  /**
   * Return an array of registered middleware
   * 
   * @param  {String} type one of: before, controller, service, fail
   * @return {Array}
   */
  self.middleware.getAll = function(type) {
    return middlewares[type];
  };


  /**
   * Load a service module
   * 
   * @param  {Object} service   the service's configuration object
   * @return {Function|Null}
   */
  self.getService = function(service) {

    try {

      // Load the service module
      var serviceTasks = require(__dirname + '/app/services/' + service.name + '.' + service.method + '.js');

      return function(req, res) {

        di.tasks(serviceTasks, req, res);

      };

    } catch (e) {

      console.error('[X] Faild to load the service ' + service.name + '.' + service.method);

      return null;

    }

  };

  /**
   * Apply middlewares and mount routes
   */
  self.mount = function() {

    var services = di.config.getServices();

    ////////////////////////////////////////////////////
    // Before Middlewares //////////////////////////////
    ////////////////////////////////////////////////////

    try {
      app.use.apply(app, middlewares.before);
    } catch (e) {
      console.error('[X] Faild to mount `before` middlewares');
    }

    ////////////////////////////////////////////////////
    // Controller //////////////////////////////////////
    ////////////////////////////////////////////////////

    di.glob('app/controllers/**/*Ctrl.js', {}, function(er, files) {

      files.forEach(function(file) {

        // The controller
        var ctrl = require(di.path.join(__dirname, file));

        // Parse the controller name from the file's name
        var ctrlName = di.changeCase.paramCase(/.+\/(.+)Ctrl\.js/.exec(file)[1]);

        // Store a referrence to the controller
        self.controllers.push({
          name: ctrlName,
          actions: []
        });

        // Foreach member
        Object.keys(ctrl).forEach(function(key) {

          // Is function (Action)
          if (di.is.function(ctrl[key])) {

            var action = ctrl[key];

            // Array of arguments' name that the function take (execluding req, res)
            var args = di.parseFunction(action).args.slice(2);

            // Normalized function name (action)
            var actionName = di.changeCase.paramCase(key);

            if (ctrlName == 'index') {
              ctrlName = '';
            }
            
            if (actionName == 'index') {
              actionName = '';
            }

            // The path of the route
            var endpoint = di.path.resolve(`/${ctrlName}/${actionName}`);

            // Check if the function has arguments
            if (args.length) {
              // Foreach argument
              for (var i in args) {
                endpoint += `/:${args[i]}`;
              }
            }

            console.log('[*]', endpoint, 'controller is mounted');

            ctrlName   = ctrlName ? ctrlName : 'index';

            actionName = actionName ? actionName : 'index';

            // Inject a set of metadata about the controller
            app.all(endpoint, function(req, res, next) {

              req.controller = ctrlName;
              req.action = actionName;

              next();

            });

            self.controllers[self.controllers.length - 1].actions.push(actionName);

            // Route middlewares then the controller's action method
            app.all.bind(app, endpoint).apply(app, middlewares.controller.concat(function(req, res) {

              var argsValues = [];

              // Check if the function has arguments
              if (args.length) {
                // Foreach argument
                for (var i in args) {
                  argsValues.push(req.params[args[i]]);
                }
              }

              action(...argsValues, req, res);
              
            }));

          }

        });

      });

    });

    ////////////////////////////////////////////////////
    // Services ////////////////////////////////////////
    ////////////////////////////////////////////////////

    // Foreach service
    services.forEach(function(service) {
      
      var ctrl = self.getService(service);

      // Mount the route and the route middlewares
      if (di.is.not.null(ctrl)) {

        var endpoint = null;

        // Prefix the services with the version
        if (di.is.propertyDefined(service, 'version') && di.is.numeric(service.version)) {
          endpoint = '/v' + service.version + service.path;
        } else {
          endpoint = '/v1' + service.path;
        }

        // Inject a copy of the service to the req object
        app[service.method](endpoint, function(req, res, next) {
          req.service = di.deepcopy(service);
          next();
        });

        // Route middlewares then the service
        app[service.method].bind(app, endpoint).apply(app, middlewares.service.concat(function(req, res) {
          ctrl(req, new di.Response(res));
        }));

        console.log('[*]', (service.method.toUpperCase() + '      ').slice(0, 6), endpoint, 'service is mounted');

      }

    });

    ////////////////////////////////////////////////////
    // After ///////////////////////////////////////////
    ////////////////////////////////////////////////////

    self.executeAfter = function(req, data) {

      return new Promise(function(resolve, reject) {

        var res = {
          data: data
        };

        var tasks = [];

        middlewares.after.forEach(function(middleware) {
          
          tasks.push(function(callback) {

            var next = function() {
              callback();
            };

            middleware(req, res, next);
            
          });

        });

        di.async.waterfall(tasks, function(error, results) {
          resolve(res.data);
        });

      });

    };

    ////////////////////////////////////////////////////
    // Fail ////////////////////////////////////////////
    ////////////////////////////////////////////////////

    try {

      if (middlewares.fail.length) {
        app.use.apply(app, middlewares.fail);
      }

    } catch (e) {

      console.error('[X] Faild to mount `fail` middlewares');

    }

  };
  
};
