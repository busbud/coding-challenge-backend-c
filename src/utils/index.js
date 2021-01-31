/** Common utility functions.
 * @module utils/index
 */
const DEFAULT_PORT = 5000;

module.exports = {
  /**
   * Creates api error by message and status code.
   * @param {number} status - The status code
   * @param {message} message - The error message
   * @returns {Error} The error object
   */
  createError(status, message) {
    var err = new Error(message);
    err.status = status;
    return err;
  },
  /**
   * Express middleware that manage req, res and errors by allowing async method inside.
   * @param {callback} fn - Express route callback with async await support
   * @returns {Promise} The route function
   */
  asyncMiddleware(fn) {
    return (req, res, next) => {
      Promise.resolve(fn(req, res, next)).catch(next);
    };
  },
  /**
   * Sends success result to client.
   * @param {Object} res - Express res object
   * @param {*} result - Api response
   */
  resultSuccess(res, result) {
    res.json({
      suggestions: result,
    });
  },
  /**
   * Converts search word compatible with to_tsquery.
   * @param {string} q - Search word
   * @param {string} [logic='|'] - Search multiple words logic
   * @returns {string} Final search
   */
  convert2VectorSearch(q, logic='|') {
    const removedMultiplaSpace = q.replace(/\s\s+/g, " ");
    return removedMultiplaSpace
      .split(" ")
      //.map((word) => word + "*")
      .join(logic);
  },
  /**
   * Application port.
   * @returns {number} The port
   */
  getServerPort() {
    return process.env.PORT || DEFAULT_PORT;
  }
};
