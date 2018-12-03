const { sanitizeString, indexCities } = require("../utils/cities");

describe("sanitizeString", () => {
  it("should remove all whitespaces", () => {
    expect(sanitizeString("à is véry    goôd to bî ù")).toEqual(
      "aisverygoodtobiu"
    );
  });
});
