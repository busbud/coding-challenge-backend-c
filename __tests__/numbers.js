const { twoDecimalsAtMost } = require("../utils/numbers");

describe("twoDecimalsAtMost", () => {
  it("should return 2 decimal if needed", () => {
    expect(twoDecimalsAtMost(1.223)).toBe(1.22);
  });
});
