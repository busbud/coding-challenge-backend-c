/**
 * Handle not found route
 * 
 * @author Mohammad Fares <faressoft.com@gmail.com>
 */

/**
 * @param {Object}   req
 * @param {Object}   res
 * @param {Function} next
 */
module.exports = function(req, res, next) {

  // Response wrapper
  res = new di.Response(res);

  // Failed status
  res.setStatus(false);

  // Send not found message
  res.error('Not Found', {}, 404);

  next();

};
