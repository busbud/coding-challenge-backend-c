const {
  levenshteinDistance,
  sortByLevensteinDistance
} = require("../utils/text");

describe("Levenshtein", () => {
  it("should compute the lev. distance between two strings", () => {
    expect(levenshteinDistance("back", "book")).toBe(2);
  });
});
