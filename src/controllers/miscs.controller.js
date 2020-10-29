/**
 * Required Modules.
 */
const { errorHandler, HttpError } = require('../helpers');

/**
 * Get response for a resource not found.
 *
 * @param {Object} req  The HTTP request object.
 * @param {Object} res  The HTTP response object.
 * @return {Object}     An object with a message property.
 */
const resourceNotFound = (req, res) => {
  try {
    // Default status code for a resource not found.
    let statusCode = 404;
    if (req.method !== 'GET') {
      statusCode = 400;
    }

    const message = 'Oops! Something went wrong! The requested resource was not found.';
    throw new HttpError(statusCode, message);
  } catch (error) {
    errorHandler(error, res);
  }
};

/**
 * Export the controller methods.
 */
module.exports = {
  resourceNotFound,
};
