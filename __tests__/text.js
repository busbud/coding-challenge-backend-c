const {
  levenshteinDistance,
  sortByLevensteinDistance
} = require("../utils/text");

describe("Levenshtein", () => {
  it("should compute the lev. distance between two strings", () => {
    expect(levenshteinDistance("back", "book")).toBe(2);
  });
  it("should sort an arrray  according the lev distance to a pivot", () => {
    // without extracting values
    expect(
      sortByLevensteinDistance("book", ["back", "baok"], v => v.a)
    ).toEqual(["baok", "back"]);
    // with value extraction
    expect(
      sortByLevensteinDistance("book", [{ b: "back" }, { a: "baok" }], v => v.a)
    ).toEqual([{ a: "baok" }, { b: "back" }]);
  });
});
