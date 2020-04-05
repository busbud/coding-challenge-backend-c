import { CityRepositoryCachingDecorator } from "../../repositories/cityRepositoryCachingDecorator";
import { mock } from "sinon";
import { expect } from "chai";
import { City } from "../../models/city.model";
import { CitySuggestionProvider } from "../../services/citySuggestionService";

const mockCities = [
  { id: 1, name: "Montreal", lat: -40, long: -40 },
  { id: 1, name: "Montpellier", lat: -20, long: -20 },
  { id: 1, name: "Toronto", lat: -30, long: 30 }
] as City[];

describe("CitySuggestionService", function() {
  let mockGetAll;
  let citySuggestionService;

  before(() => {
    mockGetAll = mock(CityRepositoryCachingDecorator.prototype);
    mockGetAll.expects("getAll").returns(mockCities);
    citySuggestionService = new CitySuggestionProvider();
  });

  it("returns the correct suggestions when called with a name", () => {
    expect(citySuggestionService.getSuggestions("Montreal")).to.deep.equal([
      { name: "Montreal", latitude: -40, longitude: -40, score: 1 },
      { name: "Montpellier", latitude: -20, longitude: -20, score: 0.75 }
    ]);

    mockGetAll.verify();
  });

  it("returns the correct suggestions when called with name and location", () => {
    expect(
      citySuggestionService.getSuggestions("Montreal", {
        latitude: -20,
        longitude: -20
      })
    ).to.deep.equal([
      {
        latitude: -40,
        longitude: -40,
        name: "Montreal",
        score: 0.9268705346512134
      },
      {
        latitude: -20,
        longitude: -20,
        name: "Montpellier",
        score: 0.875
      }
    ]);

    mockGetAll.verify();
  });
});
