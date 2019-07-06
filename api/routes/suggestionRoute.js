module.exports = function (app) {
  var suggestionController = require('../controllers/suggestionController');

  //suggestion routes
  app.route('/suggestions')
    .get(suggestionController.get_suggestions)
};