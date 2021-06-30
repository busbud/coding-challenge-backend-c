const health = require('./health')
const suggestions = require('./suggestions')

/**
 * @function
 * @description Initialize the components
 * @returns {Object} An object containing the components
 */
module.exports = () => ({
  health,
  suggestions
})
