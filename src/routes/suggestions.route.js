/**
 * Required Modules.
 */
const { getSuggestions } = require('../controllers');

const routes = {
  '/suggestions': getSuggestions,
};

/**
 * Export the router routes.
 */
module.exports = {
  routes,
};
