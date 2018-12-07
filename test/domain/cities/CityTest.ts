import { assert } from "chai";
import Cities from "../../../src/domain/cities/City";
import City from "../../../src/domain/cities/City";

describe("City", () => {
  it("should retrieve correct values", () => {
    const city = new City("id", "name", "alternateName", 30.2, 40.1, 4200);

    assert.typeOf(city, "object");
    assert.equal(city.getPopulation(), 4200);
    assert.equal(city.getName(), "name");
    assert.equal(city.getLongitude(), 40.1);
    assert.equal(city.getLatitude(), 30.2);
    assert.equal(city.getAlternateName(), "alternateName");
  });

  it("should have a score at 0 by default", () => {
    const city = new City("id", "name", "alternateName", 30.2, 40.1, 4200);

    assert.typeOf(city, "object");
    assert.equal(city.getScore(), 0);
  });
});
