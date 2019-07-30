var expect = require("chai").expect;
const geo = require("../src/suggestions/geo");

describe("Test geo", () => {
  let abbr;

  it("returns province abbreviation when the location is in canada", () => {
    abbr = geo.stateCodeAbbrv("CA", "01");
    expect(abbr).to.equal("AB");
  });

  it("returns the admin1 when admin1 is not mapped to a province name", () => {
    abbr = geo.stateCodeAbbrv("CA", "QC");
    expect(abbr).to.equal("QC");
  });

  it("returns the state when the location is in the US", () => {
    abbr = geo.stateCodeAbbrv("US", "NY");
    expect(abbr).to.equal("NY");
  });
});
