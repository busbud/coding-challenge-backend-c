'use strict';
const cityManager = require('../modules/cityManagerModule');

/**
 * Call cityManager to reload Cities
 * @param req
 * @param res
 */
exports.reloadCities = function(req, res) {
    cityManager.loadCities();
    res.json({"status" : "ok"});
};


/**
 * Count number of availables cities
 * @param req
 * @param res
 */
exports.countCities = function(req, res) {
    res.json({"number" : global.cities.length});
};