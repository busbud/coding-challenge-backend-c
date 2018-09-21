
/**
 * Dependencies.
 */

var sigmoid = require('..');
var assert = require('assert');

/**
 * Tests.
 */

describe('sigmoid()', function() {
  it('should be a function', function() {
    assert.equal(typeof sigmoid, 'function');
  });

  it('should calculate the sigmoid for negative numbers', function() {
    assert.equal(sigmoid(-1.23), 0.22618142573054617);
  });

  it('should calculate the sigmoid for positive numbers', function() {
    assert.equal(sigmoid(3.2), 0.9608342772032357);
  });
});
