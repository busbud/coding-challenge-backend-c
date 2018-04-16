/**
 * Load and provide a wrapper for the app configurations
 * 
 * @author Mohammad Fares <faressoft.com@gmail.com>
 */

/**
 * The path of the config file
 * @type {String}
 */
var CONFIG_FILE_NAME = __dirname + '/app/config/config.json';

/**
 * To store the configurations
 * @type {Object}
 */
var config = null;

/**
 * Load and cache the configurations
 */
function sycnLoad() {

  config = JSON.parse(di.fs.readFileSync(CONFIG_FILE_NAME, 'utf8'));

}

/**
 * Get and normalize the services configurations
 * 
 * @return {Array}
 */
function getServices() {

  var services = [];

  // There are defined services
  if (di.is.propertyDefined(config, 'services')) {
    services = di.deepcopy(config.services);
  }

  // Foreach service
  for (var i in services) {

    // If no params, add empty params object
    if (di.is.not.propertyDefined(services[i], 'params')) {
      services[i].params = {};
    }

    // If no version, add version param with value '1'
    if (di.is.not.propertyDefined(services[i], 'version')) {
      services[i].version = '1';
    }

    // Is auth required, add access_token field as a required field
    if (di.is.truthy(services[i].auth)) {

      services[i].params.access_token = {
        'required': true,
        'type': 'string'
      };

    }

  }

  return services;

}

/**
 * Get the config object
 * 
 * @return {Object}
 */
function get() {

  return config;

}

/**
 * Get server's configurations
 * 
 * @return {Object}
 */
function getServer() {

  return config.server;

}

/**
 * Get suggestions's configurations
 *
 * @param  {String} type
 * @return {Object}
 */
function getSuggestions(type) {

  return di.deepcopy(config.suggestions[type]);

}

////////////////////////////////////////////////////
// Module //////////////////////////////////////////
////////////////////////////////////////////////////

module.exports = {
  sycnLoad: sycnLoad,
  getServices: getServices,
  get: get,
  getServer: getServer,
  getSuggestions: getSuggestions
};
