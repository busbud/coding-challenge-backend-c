const {
  sortByDistance,
  distanceBetween,
  numericalCoordinate
} = require("../utils/geo");

describe("numericalCoordinate", () => {
  it("should convert coordinate to numerical values", () => {
    expect(
      numericalCoordinate({
        latitude: "1.0",
        longitude: "2.0"
      })
    ).toEqual({
      latitude: 1,
      longitude: 2
    });
    expect(
      numericalCoordinate({
        latitude: "y.x",
        longitude: "2.0"
      })
    ).toEqual(null);
  });
});

describe("distanceBetween", () => {
  it("should return -1 if one of the input is not a float to String", () => {
    expect(
      distanceBetween(
        { latitude: "1.0", longitude: "1.0" },
        { latitude: "x", longitude: "2.0" }
      )
    ).toBe(-1);
  });

  it("should return the distance in kilometers for the given point", () => {
    expect(
      distanceBetween(
        { latitude: "1.0", longitude: "2.0" },
        { latitude: "3.0", longitude: "4.0" }
      )
    ).toBe(314.28368918020476);
  });
});

describe("sortByDistance", () => {
  it("should sort cities list by their distance to a pivot", () => {
    expect(
      sortByDistance({ latitude: "1.0", longitude: "2.0" }, [
        { latitude: "5.0", longitude: "6.0" },
        { latitude: "3.0", longitude: "4.0" },
        { latitude: "7.0", longitude: "8.0" }
      ])
    ).toEqual([
      { latitude: "3.0", longitude: "4.0" },
      { latitude: "5.0", longitude: "6.0" },
      { latitude: "7.0", longitude: "8.0" }
    ]);
  });
});
