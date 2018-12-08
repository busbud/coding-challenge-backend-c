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

  describe("_changeScoreBy", () => {
    it("should not have a score lower than 0", () => {
      const city = new City(
        "id",
        "alongcitynametohaveanheavypenaltytomakethistestwork",
        "country code",
        "feature code",
        44,
        10,
        4200
      );

      city._changeScoreBy("cit", 150, 150);

      //only the missing letter penalty
      assert.equal(city.getScore(), 0);
    });

    describe("by name", () => {});
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

      city._changeScoreBy(cityName.slice(0, cityName.length - 1));

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

      city._changeScoreBy(cityName.slice(0, cityName.length - 3));

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

      city._changeScoreBy(cityName.slice(0, cityName.length - 4));

      assert.equal(city.getScore(), 0.8);
    });

    describe("by distance", () => {
      it("should no have any penalty with distance lower than 150km", () => {
        const city = new City(
          "id",
          "city",
          "country code",
          "feature code",
          44,
          10,
          4200
        );

        //distance is 149km
        city._changeScoreBy("cit", 45.335, 10);

        //only the missing letter penalty
        assert.equal(city.getScore(), 0.95);
      });
    });

    it("should have a 0.05 penalty every 150km", () => {
      const city = new City(
        "id",
        "city",
        "country code",
        "feature code",
        44,
        10,
        4200
      );

      city._changeScoreBy("cit", 48, 13);

      //only the missing letter penalty
      assert.equal(city.getScore(), 0.8);
    });
  });
});
