
/********************
 * Import Modules
 *******************/

var _ = require("lodash");

var Classy = require(__dirname + "/classy.js");





/********************
 * Node Class
 *******************/

/**
 * @class Node
 *
 *  Node is the class for each node in the trie keeping all token groups and references to objects.
 * All nodes have a property @objects Array with all objects indexed by the string formed of all tokens from the branch.
 *
 * @children: Children nodes of this node
 * @objects: Objects with strings corresponding to this node
 * @cache: A top10 basic ranking of cities in order by rank, precomputed for fast retrieval
 * !! These are not from the list of objects, but from the children!!!!!
 */
var Node = Classy.extend({
  constructor: function (trieParent) {
    // Check parameters
    if (!_.isObject(trieParent)) {
      // !!! Trie must be the only one to be instanciated not Node !!!
      // Node is only there to be extended
      throw new Error("Busbud C - Node: No parent Trie instance has been passed to constructor.");
    }

    // Initialize data-structure
    this._trie = trieParent;
    this.children = {};
    this.objects = [];
    this.cache = [];
  },

  // Number of objects to keep inside the cache
  cacheSize: 10,

  // Rank constants
  rankWord: 1.0,
  rankCache: 0.5,

  // Function to be overloaded for objects ordering
  objectComparison: function (a, b) {
    return 0;
  },

  // Ranking function to be overloaded
  computeRank: function (object) {
    return 1.0;
  },

  // Format data to be added to return
  formatData: function (object) {
    return _.cloneDeep(object);
  },

  // Get next node if it exists
  existsToken: function (token) {
    if (this.children || _.isString(token)) {
      return _.isObject(this.children[token]);
    } else {
      return false;
    }
  },
  _addObject: function (tokensConsumed, tokensRemaining, object) {
    // Check parameters
    if (!(_.isArray(tokensConsumed) && _.isArray(tokensRemaining)) || !_.isObject(object)) {
      return false;
    }

    // Check if tokensRemaining has tokens
    if (tokensRemaining.size > 0) {
      // Consume token
      var _token = tokensRemaining.shift();
      tokensConsumed.push(_token);

      // Check if token is not already in children
      if (!_.isObject(this.children[_token])) {
        this.children[_token] = new this._trie.nodeClass();
      }

      // Add object to cache if he's cool enough
      var _insertedCache = false;
      for (var c = 0; c < this.cache.length; c++) {
        // Check if this object is bigger so it can be inserted
        if (this.objectComparison(object, this.cache[c]) > 0) {
          this.cache.splice(c, 0, object);
          _insertedCache = true;
        }
      }
      if (this.cache.length < this.cacheSize) {
        if (_insertedCache === false) {
          this.cache.push(object);
        }
      }

      // Check if cache is too big after we added the object
      if (this.cache.length > this.cacheSize) {
        this.cache.splice(this.cacheSize, this.cache.length - this.cacheSize);
      }

      // Continue in child the recursion
      return this.children[_token]._addObject(tokensConsumed, tokensRemaining, object);
    } else { // This node corresponds with the word
      // Search inside array the index to insert object so that objects is sorted
      var _inserted = false;
      for (var i = 0; i < this.objects.length; i++) {
        // Check if this object is bigger so it can be inserted
        if (this.objectComparison(object, this.objects[i]) > 0) {
          this.objects.splice(i, 0, object);
          _inserted = true;
        }
      }

      // If object is not inserted then add it at the end
      if (!_inserted) {
        this.objects.push(object);
      }

      // Object has been successfully added, return true
      return true;
    }
  },

  _traverse: function (tokensConsumed, tokensLeft) {
    // Check parameters
    if (!_.isArray(tokensConsumed)) {
      return [];
    }

    // Check if no more tokens are to consume
    if (tokensLeft.length === 0) {
      // We are on the word, yayyy
      var _result = [];

      // Loop trough objects if any and add them to _result
      for (var o = 0; o < this.objects.length; o++) {
        var _obj = this.formatData(this.objects[o]);

        // Add rank
        if (!_.isObject(_obj.rank)) {
          _obj.rank = {};
        }
        _obj.rank.word = this.rankWord;

        // Add object to result
        _result.push(_obj);
      }

      // Loop through cache if any and add them to result
      for (var c = 0; c < this.cache.length; c++) { // hehe c++, see what I did there?
        var _obj = this.formatData(this.objects[c]);

        // Add rank
        if (!_.isObject(_obj.rank)) {
          _obj.rank = {};
        }
        _obj.rank.word = this.rankCache;

        // Add object to result
        _result.push(_obj);
      }

      return _result
    } else {
      // Consume token
      var _token = tokensLeft.shift();
      tokensConsumed.push(_token);

      // Check if child node can be accessed with token
      if (_.isObject(this.children[_token])) {
        // Go to child
        return this.children[_token]._traverse(tokensConsumed, tokensLeft);
      } else { // If not it means we found nothing
        return [];
        // TODO: Should still return something iff 3 or more tokens have been consumed
      }
    }
  }
});




/********************
 * Trie Class
 *******************/

var Trie = Classy.extend({
  constructor: function (dataHash) {
    // Initialize data-structure
    this.parentNode = new this.nodeClass(this);

    // Check datalist and loop over if cool, this method is slow
    if (_.isObject(dataHash)) {
      for (var _str in dataHash) {
        this.addObject(_str, dataHash[_str]);
      }
    }
  },

  // The class to instanciate the Nodes
  nodeClass: Node,

  /**
   * Function to add a string to the trie as a reverse index for the bucket of objects.
   *
   * @param string: the tokens with what the trie will be initialized
   * @param object: to be added for the string
   */
  addObject: function (string, object) {
    // Chech parameters
    if (!(_.isString(string) && _.isObject(object))) {
      return false;
    }

    // Tokenize the string and give it to the node to traverse recursively
    var _tokens = string.split("");
    return this.parentNode._addObject([], _tokens, object);
  },

  /**
   * Traverse trie with string
   *
   * @param string to be split into tokens and search for all collisions
   */
  traverse: function (string) {
    // Check parameters
    if (!(_.isString(string))) {
      // Split in tokens and traverse nodes in search of treasure
      var _tokens = string.split("");
      return this.parentNode._traverse([], _tokens);
    }

    // No string, no gain
    return [];
  }

});




/********************
 * Export Modules
 *******************/

module.exports = Trie;

module.exports.Node = Node;