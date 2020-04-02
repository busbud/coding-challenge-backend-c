import { CityRepositoryCachingDecorator } from "../repositories/cityRepositoryCachingDecorator";
import { CityRepository } from "../repositories/cityRepository";
import { IRepository } from "../repositories/IRepository";
import { City } from "../models/city.model";
import { CitySuggestion } from "../models/citySuggestion.model";

export class CityService {
  private cachedRepository: IRepository<City>;

  constructor() {
    let repository = new CityRepository();
    this.cachedRepository = new CityRepositoryCachingDecorator(repository);
  }

  getByName(name: string): CitySuggestion[] {
    return this.cachedRepository
      .getAll()
      .filter(city => city.name.startsWith(name))
      .map(this.mapToSuggestion);
  }

  private mapToSuggestion(city: City) {
    return {
      name: city.name,
      latitude: city.lat,
      longitude: city.long,
      score: 0
    };
  }
}
