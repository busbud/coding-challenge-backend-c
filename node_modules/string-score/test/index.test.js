var score = require('../');

describe('string-score', function () {
  var hello = 'Hello, World!';

  describe('core functionality', function () {
    it('should return 1.0 when passed two equal strings', function () {
      expect(score('Foo', 'Foo')).to.equal(1.0);
    });

    it('should return 0.0 when passed strings with non-existing characters', function () {
      expect(score('Foo Bar', 'fuu')).to.equal(0.0);
      expect(score('Foo Bar', 'Foo_Bar')).to.equal(0.0);
    });

    it('should return 0.0 when passed an empty query', function () {
      expect(score('Foo Bar', '')).to.equal(0.0);
    });

    it('should only match sequentially', function () {
      expect(score(hello, 'WH')).to.equal(0.0);
    });

    it('should return a better score for the same case rather than the opposite case', function () {
      expect(score(hello, 'hello')).to.be.lt(score(hello, 'Hello'));
    });

    it('should return a higher score for closer matches', function () {
      expect(score(hello, 'H')).to.be.lt(score(hello, 'He'));
    });

    it('should return a match with the wrong case', function () {
      expect(score('Hillsdale, Michigan', 'himi')).to.be.gt(0.0);
    });

    it('should have proper relative weighting', function () {
      var str = hello;
      expect(score(str, 'e')).to.be.lt(score(str, 'h'));
      expect(score(str, 'h')).to.be.lt(score(str, 'he'));
      expect(score(str, 'hel')).to.be.lt(score(str, 'hell'));
      expect(score(str, 'hell')).to.be.lt(score(str, 'hello'));
      expect(score(str, 'hello')).to.be.lt(score(str, 'helloworld'));
      expect(score(str, 'hello worl')).to.be.lt(score(str, 'helloworl'));
      expect(score(str, 'hello worl')).to.be.lt(score(str, 'hello world'));
    });
  });

  describe('adanced scoring methods', function () {
    it('has a consecutive letter bonus', function () {
      expect(score(hello, 'Hel')).to.be.gt(score(hello, 'Hld'));
    });

    it('has an acronym bonus', function () {
      expect(score(hello, 'HW', 0.5)).to.be.gt(score(hello, 'Ho', 0.5));
      expect(score('Hillsdale Michigan', 'HiMi', 0.5)).to.be.gt(score('Hillsdale, Michigan', 'Hil', 0.5));
      expect(score('Hillsdale Michigan', 'HiMi', 0.5)).to.be.gt(score('Hillsdale, Michigan', 'illsda', 0.5));
      expect(score('Hillsdale Michigan', 'HiMi', 0.5)).to.be.gt(score('Hillsdale, Michigan', 'Hills', 0.5));
      expect(score('Hillsdale Michigan', 'HiMi', 0.5)).to.be.gt(score('Hillsdale, Michigan', 'hillsd', 0.5));
    });

    it('has a beginning of string bonus', function () {
      expect(score('Hillsdale', 'hi')).to.be.gt(score('Hillsdale', 'dale'));
      expect(score(hello, 'h')).to.be.gt(score(hello, 'w'));
    });

    it('has proper string weights', function () {
      expect(score('Research Resources North', 'res')).to.be.gt(score('Mary Conces', 'res'));
      expect(score('Research Resources North', 'res')).to.be.gt(score('Mary had a resourceful little lamb.', 'res'));
    });
  });

  describe('fuzzy matching', function () {
    it('should score mismatched strings', function () {
      expect(score(hello, 'Hz')).to.equal(0);
      expect(score(hello, 'Hz', 0.5)).to.be.gt(0).and.be.lt(score(hello, 'He', 0.5));
    });

    it('should be tuned well', function () {
      expect(score(hello, 'Hello, Worl', 0.5)).to.be.gt(score(hello, 'Hello, Worl1'));
      expect(score(hello, 'jello', 0.5)).to.be.gt(0);
    });

    it('should have varying degrees of fuzziness', function () {
      expect(score(hello, 'Hz', 0.9)).to.be.gt(score(hello, '0.5'));
    })
  });
});
