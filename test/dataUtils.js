var expect = require("chai").expect;
const { getRegEx, getRedisKey, sanitizeRequestParams } = require("../src/suggestions/dataUtils");

describe("Test utils", () => {
  describe("Test getRegEx function", () => {
    const dataProvider = [
      { q: "abc", expected: "/abc/i", msg: "returns valid regex" },
      { q: "a bc", expected: "/abc/i", msg: "removes spaces" },
      { q: "àbc", expected: "/abc/i", msg: "replaces accent" },
      { expected: "/(?:)/i", msg: "can handle empty query" }
    ];

    let result;
    dataProvider.forEach(({ q, expected, msg }) => {
      it(msg, () => {
        result = getRegEx(q);
        expect(result).to.be.instanceof(RegExp);
        expect(result.toString()).to.equal(expected);
      });
    });
  });  
  describe("Test getRedisKey function", () => {
    const dataProvider = [
      { query: {q: "a"}, expected: "suggestions-/a/i", msg: "without geo"},
      { query: {q: "a", long:1}, expected: "suggestions-/a/i", msg: "without lat"},
      { query: {q: "a", lat:1}, expected: "suggestions-/a/i", msg: "without long"},
      { query: {q: "a", long:1, lat: 2}, expected: "suggestions-/a/i-1-2", msg: "with geo"},      
    ];

    let result;
    dataProvider.forEach(({ query, expected, msg }) => {
      it(msg, () => {
        result = getRedisKey(query);
        expect(result).to.equal(expected);
      });
    });
  });  

  describe('Test sanitizeRequestParams function', () => {
    it('throws an error on missing q', () => {
      expect(() => sanitizeRequestParams({})).to.throw('Param q is required')
    });
    
    it('round long and lat to number', () => {
      const result = sanitizeRequestParams({q:'a', long:1.234, lat: 1.567});
      expect(result.long).to.eql(1);
      expect(result.lat).to.eql(2);
    })

    it('has no geo prop when one of the input isNaN', () => {
      const result = sanitizeRequestParams({q:'a', long:'a', lat: 1.567});
      expect(result.long).to.eql(undefined);
      expect(result.lat).to.eql(undefined);
    });

    it('has no geo prop when one of the input is missing', () => {
      const result = sanitizeRequestParams({q:'a', long:1});
      expect(result.long).to.eql(undefined);
      expect(result.lat).to.eql(undefined);
    });

  })
});
