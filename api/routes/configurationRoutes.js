'use strict';
module.exports = function(app) {
    var configurationController = require('../controllers/configurationController');

    app.route('/reloadCities')
        .get(configurationController.reloadCities);
    app.route('/countCities')
        .get(configurationController.countCities);

}