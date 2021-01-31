/** Express router switch
 * @module routes/index
 * @requires express
 */
const suggestions = require('./suggestions')

module.exports = app => {
  app.use('/suggestions', suggestions)
}