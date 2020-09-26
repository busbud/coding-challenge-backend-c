const escapeStringRegexp = require('escape-string-regexp');

const replaceSpecialCharacters = (string = '') => {
  return string.replace(/a/gi, '[a,á,à,ä]')
      .replace(/e/gi, '[e,é,ë]')
      .replace(/i/gi, '[i,í,ï]')
      .replace(/o/gi, '[o,ó,ö,ò]')
      .replace(/u/gi, '[u,ü,ú,ù]');
};

const sanitize = (string = '') => replaceSpecialCharacters(escapeStringRegexp(string));

const sanitizer = {
  escapeStringRegexp,
  replaceSpecialCharacters,
  sanitize,
};

module.exports = sanitizer;
