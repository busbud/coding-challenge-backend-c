'use strict';
module.exports = function(app) {
    var suggestionsController = require('../controllers/suggestionsController');

    app.route('/suggestions')
        .get(suggestionsController.getSuggestions);
}