var expect  = require('chai').expect;
var stringNormalization = require('../src/utils/stringNormalization');

describe('Normalize string', function() {
  describe('with a non-normalized string', function () {
    
    var s = "éaFâ";
    var response = stringNormalization.normalize(s);

    it('returns a normalized string', function () {
      expect(response).to.equal("eafa");
    });

  });

});