'use strict';

// Loading is done synchronously. If it is a problem I can make async
var { data, map } = require('./data/data');

var DB = function () {};

// search by prefix
// as all prefixes are precomputed, it is just a straightforward get from an object
// for bigger data volume, this trick will not be possible
// map is sharded by string length. Not sure if the improvment is big compared to the internal btree of the object getter
DB.prototype.find = function (query) {
	return map[query.length][query];
};

// retrieval of objects from their index found by find()
DB.prototype.get = function (index) {
	return data[index];
};

module.exports = DB;