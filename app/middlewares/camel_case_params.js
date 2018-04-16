/**
 * Change req.params and req.schemaParams keys to camelcase
 * 
 * @author Mohammad Fares <faressoft.com@gmail.com>
 */

/**
 * Change req.params and req.schemaParams keys to camelcase
 * 
 * @param {Object} req
 * @param {Object} res
 * @param {Object} next
 */
module.exports = function(req, res, next) {

  req.params = di.objectChangeCase(req.params, 'camel');
  req.schemaParams = di.objectChangeCase(req.schemaParams, 'camel');

  next();

};
