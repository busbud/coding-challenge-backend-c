import { CityRepositoryCachingDecorator } from "../repositories/cityRepositoryCachingDecorator";
import { CityRepository } from "../repositories/cityRepository";
import { IRepository } from "../repositories/IRepository";
import { City } from "../models/city.model";
import { CitySuggestion } from "../models/citySuggestion.model";
import { Searcher } from "fast-fuzzy";

type Location = { longitude: number; latitude: number };

type FuzzyResult<T> = {
  item: T;
  score: number;
  original: T;
  key: string;
  match: { index: number; length: number };
};

const EARTH_RADIUS = 6371e3;

const EARTH_CIRCUMFERENCE = 2 * Math.PI * EARTH_RADIUS;

export class CitySuggestionService {
  private cachedRepository: IRepository<City>;
  private fastSearcher: any;

  constructor() {
    let repository = new CityRepository();
    this.cachedRepository = new CityRepositoryCachingDecorator(repository);
    this.fastSearcher = new Searcher(this.cachedRepository.getAll(), {
      keySelector: (city: City) => city.name,
      returnMatchData: true,
      threshold: 0.75
    });
  }

  getSuggestions(name: string, userLocation?: Location): CitySuggestion[] {
    return this.fastSearcher
      .search(name)
      .map(userLocation ? this.applyGeoScore(userLocation) : x => x)
      .map(this.mapToSuggestion)
      .sort((a: CitySuggestion, b: CitySuggestion) => b.score - a.score);
  }

  private applyGeoScore = (userLocation: Location) => (
    matchedItem: FuzzyResult<City>
  ) => {
    const city = matchedItem.item;
    const cityLocation = {
      latitude: city.lat,
      longitude: city.long
    };

    return {
      ...matchedItem,
      score:
        (matchedItem.score +
          this.calculateGeoScore(userLocation, cityLocation)) /
        2
    };
  };

  private calculateGeoScore = (
    userLocation: Location,
    cityLocation: Location
  ) =>
    1 -
    this.calculateDistance(userLocation, cityLocation) /
      (EARTH_CIRCUMFERENCE / 2);

  private calculateDistance = (
    userLocation: Location,
    cityLocation: Location
  ) => {
    const Δφ = this.getRadians(cityLocation.latitude - userLocation.latitude);
    const Δλ = this.getRadians(cityLocation.longitude - userLocation.latitude);

    const a =
      Math.sin(Δφ / 2) * Math.sin(Δφ / 2) +
      Math.cos(this.getRadians(userLocation.latitude)) *
        Math.cos(this.getRadians(cityLocation.latitude)) *
        Math.sin(Δλ / 2) *
        Math.sin(Δλ / 2);
    const angularDistance = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));

    return EARTH_RADIUS * angularDistance;
  };

  private getRadians = (degrees: number) => (degrees * Math.PI) / 180;

  private mapToSuggestion({
    item: { name, lat, long },
    score
  }: FuzzyResult<City>) {
    return {
      name: name,
      latitude: lat,
      longitude: long,
      score: score
    };
  }
}
