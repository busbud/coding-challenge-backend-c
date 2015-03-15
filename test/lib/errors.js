var expect  = require('chai').expect;
var _  = require('lodash');

var errors = require('../../lib/errors');

describe('errors', function () {
  ['InvalidParameterError', 'GeneralError', 'NotFoundError'].forEach(function (errorName) {
    it('should expose ' + errorName, function () {
      expect(errors).to.have.property(errorName);
      var err = new errors[errorName]('an error');
      expect(err).to.be.instanceof(Error);
    });
  });
});