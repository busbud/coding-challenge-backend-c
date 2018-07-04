const expect = require("chai").expect;
const { toCity } = require("../../../src/infrastructure/persistence/cityMapper");

const cityFields = (name, state, country) => [
  "id",
  name,
  name,
  "",
  "-123.345",
  "45.0883",
  "",
  "",
  country,
  "",
  state,
  "",
  "",
  "",
  "300000"
];

describe("CityMapper", () => {
  describe("toCity", () => {
    it("should map to the right canadian state", () => {
      const result = toCity(cityFields("Toronto", "08", "CA"));
      expect(result.state).to.eq("ON");
    });
  });
});
