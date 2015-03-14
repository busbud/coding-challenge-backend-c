var SuperError = require('super-error');

module.exports = {
  // @TOCHECK: why should pass a string while subclassing?
  InvalidParameterError: SuperError.subclass('InvalidParameterError')

};