/**
 * Custom Error Handler
 * @param {String} type Error Type
 * @param {String} message Error Message
 * @param {String} status Error Status
 * @param {Object} payload Error Payload
 */
module.exports = (type, message, status = 500, payload = {}) => {
  const error = new Error(message);
  error.status = status;
  error.type = type;
  error.payload = payload;

  throw error;
};
