/**
 * A dummy service
 *
 * @author Mohammad Fares <faressoft.com@gmail.com>
 */

/**
 * Send the passed params again to the user
 *
 * @param  {Object}   req
 * @param  {Object}   res
 * @return {Function}
 */
function getParams(req, res) {

  return function(callback) {

    callback(null, req.params);

  };

}

module.exports = [

  // Send the passed params again to the user
  getParams

];
