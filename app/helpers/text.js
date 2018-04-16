/**
 * A wrapper around the texts resource to store error messages
 * 
 * @author Mohammad Fares <faressoft.com@gmail.com>
 */

/**
 * The path of the texts file
 * 
 * @type {String}
 */
var TEXTS_FILE_NAME = __dirname + '/../resources/text.json';

/**
 * To store the texts
 * 
 * @type {Object}
 */
var texts = null;

/**
 * Load and cache texts resource
 * 
 * @return {Promise}
 */
function load() {

  return new Promise(function(resolve, reject) {
      
    di.fs.readFile(TEXTS_FILE_NAME, 'utf8', function(error, result) {

      if (error) {
        return reject(error);
      }

      try {

        texts = JSON.parse(result);

      } catch (error) {

        return reject(new Error('Failed to parse the text.json resource'));

      }
      
      resolve();
      
    });
    
  });

}

/**
 * Get a text by its key
 * 
 * @param  {String} key
 * @return {String|Null}
 */
function get(key) {

  // Not found
  if (di.is.not.propertyDefined(texts, key)) {
    return null;
  }

  return texts[key];

}

////////////////////////////////////////////////////
// Module //////////////////////////////////////////
////////////////////////////////////////////////////

module.exports = {
  load: load,
  get: get
};
