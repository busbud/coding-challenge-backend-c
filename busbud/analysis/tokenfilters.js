
/********************
 * Import Modules
 *******************/

var _ = require("lodash");

// The unit test allowed me to see a bug in this library
// "Ç" translates to "c" instead of "C", and "ç" translates to "c" as well, dunno if other bugs
// I always lowercase before using fold to ascii, therefore for this project this bug is not major
var foldToAscii = require("diacritics").remove;

var RegExp = require("xregexp").XRegExp;


/********************
 * Token Filters
 *******************/

function TokenFilterLoop(tokens, filter) {
   // Check parameter and run
  if (_.isArray(tokens)) {
    // Resulting tokens
    var _result = [];

    // Loop through tokens and apply filter
    for (var i = 0; i < tokens.length; i++) {
      if (_.isString(tokens[i])) {
        // Get result from filter
        var _filterResult = filter(tokens[i]);

        // Check if expansion occurred
        if (_.isArray(_filterResult)) {
          _result = _result.concat(_filterResult);
        } else if (_filterResult) { // If not just
          _result.push(_filterResult);
        }
      }
    }

    // Return result
    return _.uniq(_result);
  } else {
    return [];
  }
}


function TokenFilterLowerCase(tokens) {
  return TokenFilterLoop(tokens, function (token) {
    return token.toLowerCase();
  });
}


function TokenFilterAsciiFolding(tokens) {
  return TokenFilterLoop(tokens, function (token) {
    return foldToAscii(token);
  });
}

var RegExpUniWords = RegExp.cache("\\p{L}+", "gi"),
  RegExpUniApostrophe = RegExp.cache("\\p{L}+'\\p{L}+", "gi"),
  RegExpUniDash = RegExp.cache("\\p{L}+\\-\\p{L}+", "gi");

function TokenFilterUnicodeExpandSeparators(tokens) {
  return TokenFilterLoop(tokens, function (token) {
    /*var _tokensWord = token.match(/[\w]+/ig),
      _tokensApostrophe = token.match(/\w+'\w+/ig),
      _tokensDash = token.match(/\w+\-\w+/ig),
      _result = [].concat(token); */

    var _result = [].concat(token);

    RegExp.forEach(token, RegExpUniWords, function (match) {
      if (match[0] !== token) {
        _result.push(match[0]);
      }
    }, this);
    RegExp.forEach(token, RegExpUniApostrophe, function (match) {
      if (match[0] !== token) {
        _result.push(match[0]);
      }
    }, this);
    RegExp.forEach(token, RegExpUniDash, function (match) {
      if (match[0] !== token) {
        _result.push(match[0]);
      }
    }, this);

    /* Since mathing the regex can be null, I want to return only valid
    if (_tokensWord) {
      _result = _result.concat(_tokensWord);
    }
    if (_tokensApostrophe) {
      _result = _result.concat(_tokensApostrophe);
    }
    if (_tokensDash) {
      _result = _result.concat(_tokensDash);
    } */

    return _result;
  });
}

function TokenFilterExpandSeparators(tokens) {
  return TokenFilterLoop(tokens, function (token) {
    var _tokensWord = token.match(/[\w]+/ig),
      _tokensApostrophe = token.match(/\w+'\w+/ig),
      _tokensDash = token.match(/\w+\-\w+/ig),
      _result = [].concat(token);

    var _result = [].concat(token);

    // Since mathing the regex can be null, I want to return only valid
    if (_tokensWord) {
      _result = _result.concat(_tokensWord);
    }
    if (_tokensApostrophe) {
      _result = _result.concat(_tokensApostrophe);
    }
    if (_tokensDash) {
      _result = _result.concat(_tokensDash);
    }

    return _result;
  });
}




/********************
 * Export Modules
 *******************/

module.exports = {};

module.exports.LowerCase = TokenFilterLowerCase;
module.exports.AsciiFolding = TokenFilterAsciiFolding;
module.exports.ExpandSeparators = TokenFilterExpandSeparators;
module.exports.UniExpandSeparators = TokenFilterUnicodeExpandSeparators;
