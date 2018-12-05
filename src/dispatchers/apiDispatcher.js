'use strict';
const controllers = require(_src + '/controllers');

// ################ Search ###############
/**
 * Search
 * @param req
 * @param res
 * @param next
 */
module.exports.search = function(req, res, next) {
  return controllers.search(req, res, next);
};
