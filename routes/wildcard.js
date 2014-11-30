
// wildcard Handler
//
// @param {Object} req
// @param {Object} res
//
// Usage:
// return wildcard(req, res);

var helpersNotFound = require('../helpers/responses/notFound');

module.exports = function wildcard(req, res) {
  return helpersNotFound(res);
};