/**
 * Error handling for any unhandled errors that throws from any route
 * For the production environment the stacktrace will be hidden from the end user
 * Otherwise it will be included in the output
 * 
 * @author Mohammad Fares <faressoft.com@gmail.com>
 */

/**
 * @param {Object}   error
 * @param {Object}   req
 * @param {Object}   res
 * @param {Function} next
 */
module.exports = function(error, req, res, next) {

  // Response wrapper
  res = new di.Response(res);

  // Failed status
  res.setStatus(false);

  // Print the stacktrace if the environment is not production
  if (process.env.NODE_ENV == 'production') {

    // Send generic error message
    res.error(di.text.get('SOMETHING_WRONG'), {}, 500);

  } else {

    console.error(JSON.stringify(di.serializeError(error), null, 2));

    // Send the error object
    res.error(error.message, di.serializeError(error), 500);

  }

};
