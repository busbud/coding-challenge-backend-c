const { sanitizeString, indexCities } = require("../utils/cities");

describe("sanitizeString", () => {
  it("should remove all whitespaces", () => {
    expect(sanitizeString("a is very    good to b")).toEqual("aisverygoodtob");
  });
});
