var expect  = require('chai').expect;
var _  = require('lodash');

var errors = require('../../lib/errors');

describe('error', function () {
  it('should expose InvalidParameterError', function () {
    expect(errors).to.have.property('InvalidParameterError');
    var err = new errors.InvalidParameterError('invalid');
    expect(err).to.be.instanceof(Error);
  });
});