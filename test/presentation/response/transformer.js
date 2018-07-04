const expect = require("chai").expect;
const { transform, sort } = require("../../../src/presentation/response/transformer");

describe("Transformer", () => {
  describe("transform", () => {
    it("returns transformed result", () => {
      const result = [
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

      const transformed = transform(result);

      expect(transformed[0]).to.have.property("name", "San Francisco, CA, US");
      expect(transformed[0]).to.have.property("longitude", 10);
      expect(transformed[0]).to.have.property("latitude", 20);
      expect(transformed[0]).to.have.property("score", 1);
    });
  });
});
