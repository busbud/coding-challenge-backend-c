const adapter = require('../infrastructure/suggestions/search.adapter');

module.exports.search = async (q, latitude, longitude) => adapter
  .fulltextSearch(q, latitude, longitude);
