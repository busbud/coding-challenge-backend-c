
/********************
 * Import Modules
 *******************/

var _ = require("lodash");

var RegExp = require("xregexp").XRegExp;

var foldToAscii = require("diacritics").remove;




/********************
 * Tokenizers
 *******************/

// unicode regular expression matcher for BasicTokenizer
var UnicodeRegExp = RegExp.cache("[\\p{L}'-]+", "gi");

// I wish to do it just like ElasticSearch's Standard Tokenizer, but it's a bit hardcore
// It would take a while to copy everything from Unicode Standard Annex #29
function UnicodeBasicTokenizer(string) {
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


function EuropeanNameTokenizer(string) {
  // Check parameters
  if (!_.isString(string)) {
    return [];
  }

  // Compute string
  var _asciiString = foldToAscii(string);

  // Token list to be returned
  var _strings = _asciiString.split(/[\s,]+/),
    _tokens = [];
  //console.log("Strings from EuropeanNameTokenizer");
  //console.log(_strings);

  // Get all words from strings
  for (var i = 0; i < _strings.length; i++) {
    // Get token
    var _token = _strings[i].match(/^[\w'-]+[\w'-]$/gi);
    if (_.isArray(_token)) {
      _tokens = _tokens.concat(_token);
    } else if (_token) {
      _tokens.push(_token);
    }
  }

  //console.log("Tokens from EuropeanNameTokenizer");
  //console.log(_tokens);

  return _.uniq(_tokens);
}



/********************
 * Export Modules
 *******************/

module.exports = {};

module.exports.Unicode = UnicodeBasicTokenizer;
module.exports.EuropeanName= EuropeanNameTokenizer;