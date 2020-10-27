/**
 * Required modules.
 */
const suggestions = require('./suggestions.controller');
const miscs = require('./miscs.controller');

/**
 * Export all available modules.
 */
module.exports = {
  ...suggestions,
  ...miscs,
};
