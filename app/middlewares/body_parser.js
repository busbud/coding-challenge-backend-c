/**
 * Body parser to parse urlencoded data in the body
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

  di.bodyParser.urlencoded({
    // Don't parse to arrays and objects when is extended disabled
    extended: false
  })(req, res, next);
  
};
