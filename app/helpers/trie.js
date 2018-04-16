/**
 * Trie
 * An optimized implementation for the Trie DataStructure
 * with a support for fuzziness and storing objects instead
 * of just words by specifying a specific key of the objects
 * as lookup word.
 * 
 * @author Mohammad Fares <faressoft.com@gmail.com>
 */

class Trie {

  /**
   * @constructor
   */
  constructor() {

    /**
     * The starting point of the tree,
     * doesn't represent any letter
     * @type {Object}
     */
    this._root = {};

    /**
     * To store a unique id for each find opeartion,
     * to track the taken keys to prevent duplicates
     * when fuzziness is enabled. This id is added to each
     * chosen key to indicates that it is taken.
     * 
     * No need to clear it from the keys' objects after
     * the find operation, since clearing all these values
     * is a constly operation and that is why a UUID is used
     * instead of a boolean value to have something unique for
     * each executed operation.
     *
     * An integer value is used to represents the UUID instead of strings
     * for much faster comparisons, since comparing strings
     * is a constly operation.
     *
     * Since the code runs in a single threaded,
     * we will not have any conflicts.
     * @type {Number}
     */
    this._findUUId = null;

  }

  /**
   * Add a set of letters that form a key. The param
   * `currentLetterIndex` is used instead of removing
   * the letters letter by letter from the array
   * which is a constly operation
   * 
   * @param {Array}  letters
   * @param {Array}  length             the length of the letters array
   * @param {Number} currentLetterIndex
   * @param {Object} node               the node of the last letter (parent)
   */
  _add(letters, length, currentLetterIndex, node) {

    var currentLetter = null;
    var letterNode = null;

    // All letters are added
    if (length == currentLetterIndex) {
      return node;
    }

    currentLetter = letters[currentLetterIndex];

    // The letter is not added yet
    if (typeof node[currentLetter] == 'undefined') {

      // Multiple assignment to reduce the lookup
      letterNode = node[currentLetter] = {};

      // To store the keys
      Object.defineProperty(letterNode, 'keys', {value: [], writable: true});

      // Cache the number of keys
      Object.defineProperty(letterNode, 'keysCount', {value: 0, writable: true});

      // To store the find unique id (See: this._findUUId)
      Object.defineProperty(letterNode, 'findUUId', {value: null, writable: true});

    } else {

      letterNode = node[currentLetter];

    }

    return this._add(letters, length, currentLetterIndex + 1, letterNode);

  }

  /**
   * Add a new key
   * 
   * @param {String} key
   * @param {Object} object
   */
  add(key, object) {

    // Split into an array since array is passed by refernce
    var letters = key.split('');

    // Cache the number of letters 
    var length = letters.length;

    // Add the letters recursively
    var node = this._add(letters, letters.length, 0, this._root);

    // To be stored as node key
    var keyObject = {
      key: key,
      object: object
    };

    // Store a refernce to the node
    Object.defineProperty(keyObject, 'node', {value: node});

    // Add the key
    node.keys.push(keyObject);

    // Increment the number of keys
    node.keysCount++;

  }

  /**
   * Add current node's keys
   * 
   * @param  {Object} node         the current node
   * @param  {Number} editDistance to be added to the results
   * @param  {Array}  results      to store the results
   * @param  {Number} maxResults
   * @return {Number} the remaining results capacity
   */
  _addResults(node, editDistance, results, maxResults) {

    // The number of results that can be added
    var remainingResultsCapacity = maxResults - results.length;

    // The current node has keys
    if (node.keysCount > 0) {

      // Foreach key in the current node
      for (var i = 0; i < node.keys.length; i++) {

        var newResult = null;

        // No need for more results
        if (!remainingResultsCapacity) {
          break;
        }

        // Is taken result (See: this._findUUId)
        if (node.keys[i].findUUId == this._findUUId) {
          continue;
        }

        // Update the findUUId (See: this._findUUId)
        Object.defineProperty(node.keys[i], 'findUUId', {value: this._findUUId, writable: true});

        // Create a copy of the current key
        newResult = this._deepCopy(node.keys[i]);

        // Add the meta data distance
        newResult.meta = {
          editDistance: editDistance,
          siblingKeysCount: node.keysCount - 1,
          childLettersCount: Object.keys(node.keys[i].node).length
        };

        // Add the new result to the results list
        results.push(newResult);
        remainingResultsCapacity--;

      }

    }

    return remainingResultsCapacity;

  }

  /**
   * Add current node's keys then expand it in DFS
   * to add the keys underneath.
   * 
   * @param {Object} node         the current node
   * @param {Number} editDistance to be added to the results
   * @param {Array}  results      to store the results
   * @param {Number} maxResults
   */
  _expand(node, editDistance, results, maxResults) {

    // The number of results that can be added
    var remainingResultsCapacity = maxResults - results.length;

    // No need for more results
    if (!remainingResultsCapacity) {
      return;
    }

    // Visit the node
    this._addResults(node, editDistance, results, maxResults);

    // Foreach child node letters
    for (var letter in node) {
      this._expand(node[letter], editDistance, results, maxResults)
    }

  }

  /**
   * Add a set of letters that form a key. The param
   * `currentLetterIndex` is used instead of removing
   * the letters letter by letter from the array
   * which is a constly operation
   * 
   * @param {Array}  letters
   * @param {Array}  length             the length of the letters array
   * @param {Number} currentLetterIndex
   * @param {Object} node               the node of the last letter (parent)
   * @param {Number} editDistance       the edit distance so far
   * @param {Array}  fuzzyQueue         to store the nodes to be examined in a fuzzy way
   * @param {Array}  results            to store the results
   * @param {Object} options            {fuzziness, prefixLength, maxResults}
   */
  _find(letters, length, currentLetterIndex, node, editDistance, fuzzyQueue, results, options) {

    var currentLetter = null;
    var letterNode = null;
    var letter = null;

    // Exeeded the max number of found results
    if (results.length == options.maxResults) {
      return;
    }

    // Exeeded the max edit distance
    if (editDistance > options.fuzziness) {
      return;
    }

    // All letters are examined, it is the time to collect results
    if (length == currentLetterIndex) {
      return this._expand(node, editDistance, results, options.maxResults);
    }

    currentLetter = letters[currentLetterIndex];
    letterNode = node[currentLetter];

    // The letter is found
    if (typeof letterNode != 'undefined') {
      this._find(letters, length, currentLetterIndex + 1, letterNode, editDistance, fuzzyQueue, results, options);
    }

    // No fuzziness
    if (!options.fuzziness || options.prefixLength > currentLetterIndex) {
      return;
    }

    // Fuzziness (for extra letter)
    fuzzyQueue.push({
      currentLetterIndex: currentLetterIndex + 1,
      node: node,
      editDistance: editDistance + 1
    });
    
    // Fuzziness (for missing letter)
    for (letter in node) {

      fuzzyQueue.push({
        currentLetterIndex: currentLetterIndex,
        node: node[letter],
        editDistance: editDistance + 1
      });

    }
    
  }

  /**
   * Find a list of keys that match the passed
   * key as prefix matching
   * 
   * Options:
   * 
   * - fuzziness: the maximum edit distance (Default: 0)
   * - prefixLength: the number of initial letters which will not be `fuzzified` to reduce the number of examined keys (Default: 1)
   * - maxResults: the max number of returned results (Default: 5)
   * 
   * @param  {String} key
   * @param  {Object} Options {fuzziness, prefixLength, maxResults} (Optional)
   * @return {Array}  [{key, object, meta: {editDistance, siblingKeysCount, childLettersCount}}, ...]
   */
  find(key, options) {

    // To store the results
    var results = [];

    // Split into an array since array is passed by refernce
    var letters = key.split('');

    // Cache the number of letters 
    var length = letters.length;

    // To store the nodes to be examined in a fuzzy way
    var fuzzyQueue = [];

    // Generate a unique id (See: this._findUUId)
    this._findUUId = this._generateUUID();

    // The default value for options
    if (typeof options == 'undefined') {
      options = {};
    }

    // Apply default optinos
    options = Object.assign({
      fuzziness: 0,
      prefixLength: 1,
      maxResults: 5
    }, options);

    // Exact prefix matching
    this._find(letters, length, 0, this._root, 0, fuzzyQueue, results, options);

    // Fuzziness matching
    while (fuzzyQueue.length) {

      var fuzzyRecord = fuzzyQueue.shift();
      var currentLetterIndex = fuzzyRecord.currentLetterIndex;
      var node = fuzzyRecord.node;
      var editDistance = fuzzyRecord.editDistance;

      this._find(letters, length, currentLetterIndex, node, editDistance, fuzzyQueue, results, options);

    }

    return results;

  }

  /**
   * To generate a unique id (unique within the current process)
   * 
   * @return {Number}
   */
  _generateUUID() {

    var hrTime = process.hrtime();

    return hrTime[0] * 1e9 + hrTime[1];

  }

  /**
   * Deep copy and an object or an array
   * 
   * @return {Object|Array}
   */
  _deepCopy(object) {

    var clone = {};

    // If array
    if (Object.prototype.toString.call(object) == '[object Array]') {
      return object.slice();
    }

    // If object, foreach key
    for (var key in object) {

      if (object[key] !== null && typeof object[key] == 'object') {
        clone[key] = this._deepCopy(object[key]);
      } else {
        clone[key] = object[key];
      }

    }

    return clone;

  }

}

////////////////////////////////////////////////////
// Module //////////////////////////////////////////
////////////////////////////////////////////////////

module.exports = Trie;
