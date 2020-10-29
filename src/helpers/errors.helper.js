/**
 * Custom http error constructor.
 */
class HttpError extends Error {
  constructor(statusCode, message) {
    super();
    this.statusCode = statusCode;
    this.message = message;
  }
}

/**
 * Format the error for being sent to the client.
 *
 * @param{object} error   The default error object.
 * @param{res} res        The http response object.
 * @return{httpResponse}  The HTTP response object
 */
const errorHandler = (error, res) => {
  const { statusCode, message } = error;

  res.statusCode = statusCode || 500;
  res.end(JSON.stringify({ message }));
};

/**
 * Export all available modules.
 */
module.exports = {
  HttpError,
  errorHandler,
};
