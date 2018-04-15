/**
 * To enable Cross-origin resource sharing (CORS)
 * 
 * @author Mohammad Fares <faressoft.com@gmail.com>
 */

/**
 * To enable Cross-origin resource sharing (CORS)
 * 
 * @param {Object} req
 * @param {Object} res
 * @param {Object} next
 */
module.exports = function(req, res, next) {

  res.header('Access-Control-Allow-Origin', '*');
  res.header('Access-Control-Allow-Headers', 'X-Requested-With');
  res.header('Access-Control-Allow-Methods', 'GET, POST, DELETE, PUT');

  next();

};
