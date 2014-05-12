
/********************
 * Import Modules
 *******************/

var _ = require("lodash");

var Classy = require(__dirname + "/../structures/classy.js");




/********************
 * Analyzer Class
 *******************/

var Analyzer = Classy.extend({
  constructor: function () {},

  tokenizer: function (string) {
    return [].push(string);
  },

  tokenFilters: [],

  analyze: function (string) {
    // Check parameters
    if (_.isString(string)) {
      // Apply tokenizer
      var _tokens = _.uniq(this.tokenizer(string));

      // Loop through token filters
      for (var i = 0; i < this.tokenFilters.length; i++) {
        // Check if token filter is a function
        if (_.isFunction(this.tokenFilters[i])) {
          //console.log("Tokens:");
          //console.log(_tokens);
          _tokens = _.uniq(this.tokenFilters[i](_tokens));
        } else {
          console.log("Busbud C - Analyzer: Bad token filter given. Not a function.");
        }
      }

      // Return analyzed string in tokens
      return _tokens;
    } else {
      return null;
    }
  }
});




/********************
 * Export Modules
 *******************/

module.exports = Analyzer;
