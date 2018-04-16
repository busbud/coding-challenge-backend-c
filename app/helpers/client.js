/**
 * To make internal calls
 * 
 * @author Mohammad Fares <faressoft.com@gmail.com>
 */

/**
 * To make internal calls
 * 
 * @param {Router} router
 */
module.exports = function(router) {

  var self = this;

  var services = di.config.getServices();

  var middlewares = router.middleware.getAll('before')
      .concat(router.middleware.getAll('service'));

  /**
   * Make an internal  call
   * 
   * @param  {String}  method
   * @param  {String}  endpoint
   * @param  {Object}  params
   * @param  {Boolean} rejectMode default: true, reject the requests with status: false
   * @return {Promise}
   */
  var call = function(method, endpoint, params, rejectMode) {

    if (typeof rejectMode === 'undefined') {
      rejectMode = true;
    }

    endpoint = self.pathReplace(endpoint, params);
    
    return new Promise(function(resolve, reject) {

      var pathMatch = endpoint.match(/\/v(\d+)(\/.+)/);
      var version = pathMatch[1];
      var path = pathMatch[2];
      var service = null;

      // Find the service
      for (var i in services) {

        // Check method match
        if (services[i].method !== method) {
          continue;
        }

        // Check version match
        if (services[i].version !== version) {
          continue;
        }

        var match = di.routePattern(services[i].path).match(path);

        // Check path match
        if (match) {

          // Merge params with pathParams
          params = di.merge(params, match.pathParams);

          // Reference to the current service
          service = di.deepcopy(services[i]);

          break;

        }

      }

      // Service not found
      if (!service) {
        return reject(new Error(di.text.get('SERIVCE_NOT_FOUND')));
      }

      // Virtual req object
      var req = {
        path: endpoint,
        body: {},
        query: {},
        params: params,
        service: service
      };

      // To indicate if one of the middleware interrupt the flow and sended the data
      var sended = false;

      // Wrapper for the response object to catch the sended data
      var res = {
        send: function(data) {

          sended = true;

          if (rejectMode && !data.meta.status) {
            return reject(new Error(data.meta.message));
          }

          resolve(data);
        }
      };

      var pointer = 0;

      /**
       * Next
       */
      var next = function() {

        if (sended) {
          console.log('Sended');
          return; 
        }

        if (pointer <= middlewares.length) {

          try {
            var i = pointer++;
            middlewares[i](req, res, next);
          } catch (e) {
            next();
          }

        } else {

          res = new di.Response({
            send: function(data) {

              if (rejectMode && !data.meta.status) {
                return reject(new Error(data.meta.message));
              }

              resolve(data);
            }
          });

          router.getService(service)(req, res);

        }

      };

      next();

    });

  };

  self.get = function(endpoint, params, rejectMode) {
    return call('get', endpoint, params, rejectMode);
  };

  self.post = function(endpoint, params, rejectMode) {
    return call('post', endpoint, params, rejectMode);
  };

  self.put = function(endpoint, params, rejectMode) {
    return call('put', endpoint, params, rejectMode);
  };

  self.delete = function(endpoint, params, rejectMode) {
    return call('delete', endpoint, params, rejectMode);
  };

  /**
   * To replace url's placeholders with its values,
   * And delete the replaced parameters from the passed object
   * 
   * @param  {String} endpoint
   * @param  {Object} params
   * @return {String}
   */
  self.pathReplace = function(endpoint, params) {

    for (var i in params) {

      var value = params[i];

      // Param at the middle
      var pattern = new RegExp('/:' + i + '/');

      if (pattern.test(endpoint)) {
        endpoint = endpoint.replace(pattern, '/' + value + '/');
        delete params[i];
      }

      // Param at the end
      pattern = new RegExp('/:' + i + '$');

      if (pattern.test(endpoint)) {
        endpoint = endpoint.replace(pattern, '/' + value);
        delete params[i];
      }

    }

    return endpoint;

  };

};
