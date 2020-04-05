import { mock } from "sinon";
import { expect } from "chai";
import * as fs from "fs";
import { CityRepository } from "../../repositories/cityRepository";
import { CityRepositoryCachingDecorator } from "../../repositories/cityRepositoryCachingDecorator";

describe("CityRepositoryCachingDecorator", function() {
  describe("When getAll is called", () => {
    let mockRepository;
    let cachedCityRepository;

    before(() => {
      mockRepository = mock(CityRepository.prototype);
      mockRepository
        .expects("getAll")
        .returns([])
        .once();
      cachedCityRepository = new CityRepositoryCachingDecorator(
        new CityRepository()
      );
    });

    after(() => {
      mockRepository.restore();
    });

    it("delegates to cityRepository the first time", () => {
      cachedCityRepository.getAll();

      mockRepository.verify();
    });

    it("does not delegate to cityRepository after the first time", () => {
      cachedCityRepository.getAll();
      cachedCityRepository.getAll();

      mockRepository.verify();
    });
  });
});
