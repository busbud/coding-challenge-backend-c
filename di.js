/**
 * Dependency Injection
 * 
 * @author Mohammad Fares <faressoft.com@gmail.com>
 */

/**
 * Dependencies
 * @type {Object}
 */
var dependency = {};

/**
 * Get a specefiec dependency
 * 
 * @param  {String}      key
 * @return {Object|Null} return null if not found
 */
var get = function(key) {

  // Not found
  if (!(key in dependency)) {
    return null;
  }

  return dependency[key];
  
};

/**
 * The names of the member methods of the di's api
 * @type {Array}
 */
var api = [];

/**
 * Get all dependencies
 * 
 * @return {Object}
 */
var getAll = function() {

  return dependency;

};

/**
 * Set/Add a dependency
 * 
 * @param {String} key
 * @param {Object} value
 */
var set = function(key, value) {

  dependency[key] = value;

  // Check if the name makes a conflict with an existing method of the di's api
  if (api.indexOf(key) !== -1) {
    return console.log('[X] You can\'t name your dependency \`' + key + '\` with a name of any of the di\'s api methods');
  }

  module.exports[key] = value;

};

/**
 * Check if a specific dependency exists
 * 
 * @param  {String}  key
 * @return {Boolean}
 */
var exists = function(key) {
  return is.propertyDefined(dependency, key);
};

////////////////////////////////////////////////////
// Module //////////////////////////////////////////
////////////////////////////////////////////////////

module.exports = {
  get: get,
  getAll: getAll,
  set: set,
  exists: exists
};

api = Object.keys(module.exports);
