'use strict';
module.exports = function(app) {
    var suggestionsController = require('../controllers/suggestionsController');

    // todoList Routes
    app.route('/suggestions')
        .get(suggestionsController.getSuggestions);
}