const expect = require("chai").expect;
const { scoreName, scoreDistance } = require("../../src/infrastructure/score");

describe("Score", () => {
  describe("scoreName", () => {
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

  describe("scoreDistance", () => {
    const METERS_IN_KM = 1000;
    it("should give a score of 1 if distance is 0", () => {
      expect(scoreDistance(0, 100)).to.eq(1);
    });

    it("should give a score of 0 if distance is equal to radius", () => {
      expect(scoreDistance(100 * METERS_IN_KM, 100)).to.eq(0);
    });

    it("should give a score between 0 and 1 if distance is within the radius", () => {
      expect(scoreDistance(47 * METERS_IN_KM, 100))
        .to.be.above(0)
        .and.below(1);
    });
  });
});
