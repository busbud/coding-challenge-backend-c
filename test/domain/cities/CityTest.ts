import { assert } from "chai";
import Cities from "../../../src/domain/cities/City";
import City from "../../../src/domain/cities/City";

describe("City", () => {
  it("should retrieve correct values", () => {
    const city = new City(
      "id",
      "name",
      "country code",
      "feature code",
      30.2,
      40.1,
      4200
    );

    assert.typeOf(city, "object");
    assert.equal(city.getPopulation(), 4200);
    assert.equal(city.getName(), "name");
    assert.equal(city.getLongitude(), 40.1);
    assert.equal(city.getLatitude(), 30.2);
    assert.equal(city.getCountryCode(), "country code");
    assert.equal(city.getFeatureCode(), "feature code");
  });

  it("should have a score at 0 by default", () => {
    const city = new City(
      "id",
      "name",
      "country code",
      "feature code",
      30.2,
      40.1,
      4200
    );

    assert.typeOf(city, "object");
    assert.equal(city.getScore(), 0);
  });

  describe("scoring", () => {
    it("score should be to 1 if perfect name match either with a great distance", () => {
      const cityName = "Bordeaux";
      const city = new City(
        "id",
        cityName,
        "country code",
        "feature code",
        30.2,
        40.1,
        4200
      );

      city._changeScoreBy(cityName, 20, 200);

      assert.equal(city.getScore(), 1);
    });

    it("should loose 0.05 point if 1 letter is missing (it's not a match !)", () => {
      const cityName = "Bordeaux";
      const city = new City(
        "id",
        cityName,
        "country code",
        "feature code",
        30.2,
        40.1,
        4200
      );

      city._changeScoreBy(cityName.slice(0, cityName.length - 1), 30.2, 40.1);

      assert.equal(city.getScore(), 0.95);
    });

    it("should loose 0.15 point if 3 letters are missing", () => {
      const cityName = "Bordeaux";
      const city = new City(
        "id",
        cityName,
        "country code",
        "feature code",
        30.2,
        40.1,
        4200
      );

      city._changeScoreBy(cityName.slice(0, cityName.length - 3), 30.2, 40.1);

      assert.equal(city.getScore(), 0.85);
    });

    it("should loose 0.2 point if 4 letters are missing", () => {
      const cityName = "Bordeaux";
      const city = new City(
        "id",
        cityName,
        "country code",
        "feature code",
        30.2,
        40.1,
        4200
      );

      city._changeScoreBy(cityName.slice(0, cityName.length - 4), 30.2, 40.1);

      assert.equal(city.getScore(), 0.8);
    });
  });
});
