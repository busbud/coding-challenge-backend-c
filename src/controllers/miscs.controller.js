/**
 * Get a list of suggestions.
 *
 * @param {Object} req  The HTTP request object.
 * @param {Object} res  The HTTP response object.
 * @return {Object}     An object with a message property.
 */
const getPageNotFound = async (req, res) => {
  const message = 'Oops! Something went wrong! The requested resource was not found.';
  res.statusCode = 404;
  res.end(JSON.stringify({ message }));
};

/**
 * Export the controller methods.
 */
module.exports = {
  getPageNotFound,
};
