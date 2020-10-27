/**
 * Get response for a resource not found.
 *
 * @param {Object} req  The HTTP request object.
 * @param {Object} res  The HTTP response object.
 * @return {Object}     An object with a message property.
 */
const resourceNotFound = async (req, res) => {
  // Default status code for a resource not found.
  let statusCode = 404;
  if (req.method !== 'GET') {
    statusCode = 400;
  }

  const message = 'Oops! Something went wrong! The requested resource was not found.';
  res.statusCode = statusCode;
  res.end(JSON.stringify({ message }));
};

/**
 * Export the controller methods.
 */
module.exports = {
  resourceNotFound,
};
