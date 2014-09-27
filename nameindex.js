"use strict";
/*jslint */
/*globals exports */
// The name index is implemented as a trie
// and allows to find complete words from incomplete
// words in an efficient way
function NameIndex() {
    this.content = {};
    this.values = [];
}
// index a key,value pair in the trie
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
// lookup a potentially incomplete name in the trie
NameIndex.prototype.lookup = function (name, deep, limitDeep) {
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
