const expect = require("chai").expect;
const { scoreName } = require("../../src/infrastructure/score");

describe("Score", () => {
  it("should give a score of 1 if exact match", () => {
    expect(scoreName("Montreal", "Montreal")).to.eq(1);
  });

  it("should give a score of 0 if not matching at all", () => {
    expect(scoreName("Montreal", "zzzzzzzzzzzzz")).to.eq(0);
  });

  it("should give a score between 0 and 1 if partial matching", () => {
    expect(scoreName("Montreal", "Montr"))
      .to.be.above(0)
      .and.below(1);
  });
});
