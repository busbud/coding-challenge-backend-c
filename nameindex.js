"use strict";
/*jslint */
/*globals exports */
// The name index is implemented as a trie
// and allows to find complete words from incomplete
// words in an efficient way

/**
 * Constructs an index for looking up partial names
 *
 * @param {String} s string to lookup
 * @param {Number} latitude optional latitude of the city fo find
 * @param {Number} longitude optional longitude of the city to find
 * @return {Array} Array of { name: "", latitude: "", longitude: "", score: 1 } objects.
 * @api public
 */
function NameIndex() {
    this.content = {};
    this.values = [];
}
/**
 * Puts a value in the index
 *
 * @param {String} name key
 * @param {Object} value to index
 * @api public
 */
NameIndex.prototype.index = function (name, value) {
    var k = name[0],
        suffix = name.slice(1),
        bucket = this.content[k];
    if (!bucket) {
        bucket = new NameIndex();
        this.content[k] = bucket;
    }
    if (suffix) {
        bucket.index(suffix, value);
    } else {
        bucket.values.push(value);
    }
    return this;
};
// retrieve all values (in descendents)
NameIndex.prototype.getAllValues = function (res) {
    var content = this.content;
    this.values.forEach(function (v) {
        res.push(v);
    });
    Object.keys(content).forEach(function (k) {
        content[k].getAllValues(res);
    });
    return res;
};
// find a bucket in the trie (i.e. a subtrie)
NameIndex.prototype.lookupBucket = function (name) {
    var k = name[0],
        bucket = this.content[k],
        suffix = name.slice(1);
    if (bucket && suffix) {
        return bucket.lookupBucket(suffix);
    }
    return bucket;
};
/**
 * Looks up a potentially incomplete name in the index.
 *
 * @param {String} name string to lookup
 * @param {Boolean} deep false for exact match only, true for partial match
 * @return {Array} Array of indexed values.
 * @api public
 */
NameIndex.prototype.lookup = function (name, deep) {
    var bucket = this.lookupBucket(name);
    if (!bucket) {
        return [];
    }
    if (deep) {
        return bucket.getAllValues([]);
    }
    return bucket.value;
};
exports.NameIndex = NameIndex;
