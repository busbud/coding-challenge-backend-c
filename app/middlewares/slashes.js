/**
 * Remove trailing slashes
 *
 * @author Mohammad Fares <faressoft.com@gmail.com>
 */

/**
 * Remove trailing slashes
 * 
 * @param {Object} req
 * @param {Object} res
 * @param {Object} next
 */
module.exports = function(req, res, next) {

  di.slashes(false)(req, res, next);
  
};
