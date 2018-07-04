const expect = require("chai").expect;
const { scoreName, scoreDistance, scoring, sort } = require("../../src/infrastructure/score");

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

  describe("scoring", () => {
    it("should give a score of 1 if both score are 1", () => {
      expect(scoring({ scoringName: 1, scoringDistance: 1 })).to.eq(1);
    });

    it("should give a score of 0 if both score are 0", () => {
      expect(scoring({ scoringName: 0, scoringDistance: 0 })).to.eq(0);
    });

    it("should use scoringName with full weight if scoringDistance is not present", () => {
      expect(scoring({ scoringName: 0.567 })).to.eq(0.567);
    });

    it("should give more weight to the distance than name", () => {
      expect(scoring({ scoringName: 0, scoringDistance: 1 })).to.eq(0.7);
    });
  });

  describe("sort", () => {
    it("returns sorted result by descending order", () => {
      const result = [
        {
          name: "San Bernardino",
          nameAscii: "San Bernardino",
          state: "CA",
          country: "US",
          location: { longitude: 30, latitude: 40 },
          score: 0,
          scoringDistance: 0,
          scoringName: 00
        },
        {
          name: "San Francisco",
          nameAscii: "San Francisco",
          state: "CA",
          country: "US",
          location: { longitude: 10, latitude: 20 },
          score: 1,
          scoringDistance: 0.5,
          scoringName: 0.6
        }
      ];

      const sorted = result.sort(sort);

      expect(sorted[0]).to.have.property("score", 1);
      expect(sorted[1]).to.have.property("score", 0);
    });
  });
});
