/**
 * Get a list of avaliable api services and their details
 *
 * @author Mohammad Fares <faressoft.com@gmail.com>
 */

/**
 * Get a list of avaliable api services and their details
 * 
 * @param  {Object}   req
 * @param  {Object}   res
 * @return {Function}
 */
function getServices(req, res) {

  return function(callback) {

    // Async reading tasks for services' scripts
    var tasks = [];

    var services = di.deepcopy(di.config.getServices());

    // Foreach service
    services.forEach(function(service) {
      
      var servicePath = di.path.join(__dirname, service.name + '.' + service.method + '.js');
      
      tasks.push(function(callback) {

        di.fs.readFile(servicePath, {encoding: 'utf-8'}, function(error, serviceScript) {

          if (error) {
            return callback(error);
          }

          service.description = di.commentParser(serviceScript)[0].description;

          // Take only the first line of the description
          if (service.description.length) {
            service.description = service.description.split('\n')[0];
          }

          // Remove the access token param if exists 
          if (typeof service.params.access_token !== 'undefined') {
            delete service.params.access_token;
          }

          callback(null, service);
          
        });

      });

    });

    di.async.parallel(tasks, function(error, services) {

      if (error) {
        return callback(di.text.get('SOMETHING_WRONG'));
      }

      return callback(null, services);
      
    });

  };

}

module.exports = [

  // Get a list of avaliable api services and their details
  getServices

];
