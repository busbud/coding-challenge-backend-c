const _ = require('lodash');

module.exports.normalize = (str) => {
  return _.deburr(str)
    .replace(/[-,’'_.+&]/g, ' ')
    .replace(/\s+/g, ' ')
    .replace(/[^\w\s]/ig, '')
    .trim().toLowerCase();
};