import { assert } from "chai";
import Cities from "../../../src/domain/cities/Cities";
import City from "../../../src/domain/cities/City";

describe("Cities", () => {
  it("should instanciate with no cities", () => {
    const cities = Cities.newWithoutCities();

    assert.typeOf(cities, "object");
    assert.lengthOf(cities.getCities(), 0);
  });

  it("should add a city with correct values", () => {
    const city = new City(
      "id",
      "name",
      "country code",
      "feature code",
      30.2,
      40.1,
      4200
    );
    const cities = Cities.newWithoutCities();

    cities.addCity(city);

    assert.deepEqual(cities.getCities(), [city]);
  });

  describe("autocomplete", () => {
    it("should return empty result if empty name", () => {
      const city = new City(
        "id",
        "name",
        "country code",
        "feature code",
        30.2,
        40.1,
        4200
      );
      const cities = Cities.newWithoutCities();
      cities.addCity(city);

      const result = cities.thatAutocompleteWith("");

      assert.lengthOf(result, 0);
    });

    it("should return empty result if no name matched", () => {
      const city = new City(
        "id",
        "name",
        "country code",
        "feature code",
        30.2,
        40.1,
        4200
      );
      const cities = Cities.newWithoutCities();
      cities.addCity(city);

      const result = cities.thatAutocompleteWith("Alooooooooooooooogname");

      assert.lengthOf(result, 0);
    });
  });
});
