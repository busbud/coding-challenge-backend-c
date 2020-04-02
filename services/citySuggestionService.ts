import { CityRepositoryCachingDecorator } from "../repositories/cityRepositoryCachingDecorator";
import { CityRepository } from "../repositories/cityRepository";
import { IRepository } from "../repositories/IRepository";
import { City } from "../models/city.model";
import { CitySuggestion } from "../models/citySuggestion.model";
import { search } from "fast-fuzzy";

type Location = { longitude: number; latitude: number };

export class CitySuggestionService {
  private cachedRepository: IRepository<City>;

  constructor() {
    let repository = new CityRepository();
    this.cachedRepository = new CityRepositoryCachingDecorator(repository);
  }

  getSuggestions(name: string, location?: Location): CitySuggestion[] {
    return search(name, this.cachedRepository.getAll(), {
      keySelector: (city: City) => city.name
    })
      .map(this.addScore(location))
      .map(this.mapToSuggestion);
  }

  private addScore = (location: Location) => (city: City) => ({
    ...city,
    score: 1
  });

  private mapToSuggestion(scoredCity: City & { score: number }) {
    return {
      name: scoredCity.name,
      latitude: scoredCity.lat,
      longitude: scoredCity.long,
      score: scoredCity.score
    };
  }
}
