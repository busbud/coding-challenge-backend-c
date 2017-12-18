'use strict';

// Loading is done synchronously. If it is a problem I can make async
var { data, map } = require('../data/data');

var DB = function () {};

/**
 * Search by prefix
 * 
 * As all prefixes are precomputed, it is just a straightforward get from an object
 * For bigger data volume, this trick will not be possible
 * Map is sharded by string length. Not sure if the improvment is big compared to the internal btree of the object getter
 * 
 * @param {string} query Prefix to search
 * @return {array} Array of ids for cities 
 */
DB.prototype.find = function (query) {
	return map[query.length][query];
};

/**
 * Retrieval of cities by index
 * 
 * @param {int} index Id of city
 * @returns {object} City object
 */
DB.prototype.get = function (index) {
	return data[index];
};

module.exports = DB;