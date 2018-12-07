import { assert } from "chai";
import Cities from "../../../src/domain/cities/City";
import City from "../../../src/domain/cities/City";

describe("City", () => {
  it("should retrieve correct population value", () => {
    const city = new City("id", "name", "alternateName", 30.2, 40.1, 4200);

    assert.typeOf(city, "object");
    assert.equal(city.getPopulation(), 4200);
  });
});
