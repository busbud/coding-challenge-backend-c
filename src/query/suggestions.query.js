const { defaultSearchAdapter } = require('../../config');
// eslint-disable-next-line import/no-dynamic-require
const adapter = require(defaultSearchAdapter);

module.exports.search = async (q, latitude, longitude) => adapter
  .fulltextSearch(q, latitude, longitude);
