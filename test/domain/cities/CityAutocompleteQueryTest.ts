import { assert } from "chai";
import CityAutocompleteQuery from "../../../src/domain/cities/CityAutocompleteQuery";

describe("CityAutocompleteQuery", () => {
  it("should instanciate correctly", () => {
    const query = new CityAutocompleteQuery("name", 20, 20);
  });
});
