var expect = require("chai").expect;
const { getRegEx } = require("../src/suggestions/dataUtils");

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
});
