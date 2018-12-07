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
    const city = new City("id", "name", "alternateName", 30.2, 40.1, 4200);
    const cities = Cities.newWithoutCities();

    cities.addCity(city);

    assert.deepEqual(cities.getCities(), [city]);
  });
});
