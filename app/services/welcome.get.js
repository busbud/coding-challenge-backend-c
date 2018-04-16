/**
 * Get a greeting message
 *
 * @author Mohammad Fares <faressoft.com@gmail.com>
 */

/**
 * Get a greeting message 
 *
 * @param  {Object}   req
 * @param  {Object}   res
 * @return {Function}
 */
function getGreetingMessage(req, res) {

  return function(callback) {

    callback(null, {
      message: 'Welcome ' + req.params.name
    });

  };

}

module.exports = [

  // Get a greeting message
  getGreetingMessage

];
