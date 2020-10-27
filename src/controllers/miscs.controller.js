/**
 * Get response for a page not found.
 *
 * @param {Object} req  The HTTP request object.
 * @param {Object} res  The HTTP response object.
 * @return {Object}     An object with a message property.
 */
const getPageNotFound = async (req, res) => {
  // Default status code for a page not found.
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
  getPageNotFound,
};
