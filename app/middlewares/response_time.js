/**
 * Adds a X-Response-Time header to the response
 * 
 * @author Mohammad Fares <faressoft.com@gmail.com>
 */

/**
 * Body parser to parse urlencoded data in the body
 * 
 * @param {Object} req
 * @param {Object} res
 * @param {Object} next
 */
module.exports = function(req, res, next) {

  di.responseTime()(req, res, next);
  
};
