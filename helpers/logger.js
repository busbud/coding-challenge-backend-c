// logger function
//
// @param {string} title
// @return {function}
//
var debug = require('debug');
var util = require('util');
var DEBUG_MODE = process.env.DEBUG_MODE || 'false';

module.exports = function logger(space) {
  var log = debug('suggestions:'+space);
  return function(str, str1, str2, str3){
    if (str3 != undefined) {
      log(util.format(str,str1,str2, str3));
    } else if (str2 != undefined) {
      log(util.format(str,str1,str2));
    } else if (str1 != undefined) {
      log(util.format(str,str1));
    } else {
      log(util.format(str));
    }
  }

}

