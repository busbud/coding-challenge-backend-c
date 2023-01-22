import axios from "axios";
import { CitiesService } from "../citiesService";

import chai from "chai";
const { expect } = chai;

describe("CitiesService", () => {
  describe("getCitiesByQueryParam", () => {
    it(`should return suggested cities for valid city name query`, async () => {
      const mockCityNameParam = "tor";
      const cityService = new CitiesService(axios);

      const response = await cityService.getCitiesByQueryParam({
        cityName: mockCityNameParam,
      });

      expect(response[0].name.toLowerCase()).to.include(mockCityNameParam);
      expect(response.length).to.be.greaterThan(0);
    });

    it(`should return empty array of suggestions for invalid city name`, async () => {
      const mockInvalidCityNameParam = "invalid";
      const cityService = new CitiesService(axios);

      const response = await cityService.getCitiesByQueryParam({
        cityName: mockInvalidCityNameParam,
      });

      expect(response.length).to.equal(0);
    });
  });
});
