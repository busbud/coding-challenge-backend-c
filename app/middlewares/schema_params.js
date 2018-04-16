/**
 * To inject schemaParams object to the req object
 * 
 * @author Mohammad Fares <faressoft.com@gmail.com>
 */

/**
 * To inject schemaParams object to the req object
 * - Exclude the access_token param
 * 
 * @param {Object} req
 * @param {Object} res
 * @param {Object} next
 */
module.exports = function(req, res, next) {

  var schemaParams = {};

  // Foreach param in the service's schema
  for (var paramName in req.service.params) {

    // Check if the param is passed
    if (req.params[paramName]) {
      schemaParams[paramName] = req.params[paramName];
    }

  }

  req.schemaParams = schemaParams;

  // Remove the access token attribute
  delete req.schemaParams['access_token'];

  next();

};
