
/********************
 * Import Modules
 *******************/

var _ = require("lodash");

var RegExp = require("xregexp").XRegExp;




/********************
 * Standard Tokenizer
 *******************/

// unicode regular expression matcher for BasicTokenizer
var UnicodeRegExp = RegExp.cache("[\\p{L}'-]+", "gi");

// I wish to do it just like ElasticSearch's Standard Tokenizer, but it's a bit hardcore
// It would take a while to copy everything from Unicode Standard Annex #29
function BasicTokenizer(string) {
  // Check parameters
  if (!_.isString(string)) {
    return [];
  }

  // Token list to be returned
  var _tokens = [];

  // Try to get all words
  //_tokens = string.match(/[\w'-]+/ig);
  RegExp.forEach(string, UnicodeRegExp, function (match) {
    _tokens.push(match[0]);
  }, this);

  return _tokens;
}




/********************
 * Export Modules
 *******************/

module.exports = BasicTokenizer;
