/**
 * Custom Error classes
 */
var SuperError = require('super-error');

module.exports = {
  // @TOCHECK: why should pass a string while subclassing?
  InvalidParameterError: SuperError.subclass('InvalidParameterError'),
  GeneralError: SuperError.subclass('GeneralError'),
  NotFoundError: SuperError.subclass('NotFoundError')
};