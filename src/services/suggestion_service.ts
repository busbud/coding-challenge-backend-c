import type {GetSuggestionParams, ICityRawData, IGetCitySuggestion,} from '../interfaces/interfaces';
import {scoreByDistance, scoreByNameSimilarity, sortByScore} from '../utils/scoring.util';
import NodeCache from 'node-cache';

/** Service to get suggestions pulled from cities data
 * @constructor
 * @param {ICityRawData[]} cities - cities data
 * */
export class CitiesSuggestionService {
  #citiesData: ICityRawData[];
  #cache = new NodeCache({ deleteOnExpire: true });

  constructor(cities: ICityRawData[]) {
    this.#citiesData = cities;
  }

  /** gets suggestions from cache or evaluate new ones */
  public get(params: GetSuggestionParams): IGetCitySuggestion[] {
    let cacheKey = params.q;
    cacheKey += params.latitude && params.longitude ? params.longitude + params.latitude : '';
    if (this.#cache.has(cacheKey)) {
      const hit =  this.#cache.get<IGetCitySuggestion[] | undefined>(cacheKey);
      if (hit) {
        return hit
      }
    }

    const data = this.getSuggestions(params);
    this.#cache.set(cacheKey, data, 100);
    return data;

  }

  /** evaluates new suggestions */
  private getSuggestions(params: GetSuggestionParams) {
    if (params.latitude && params.longitude) {
      return this.getSuggestionsWithCoordinates(
        params.q,
        params.latitude,
        params.longitude,
        5000,
        ['CA', 'US'],
      );
    } else {
      return this.getCities(params.q, 5000, ['CA', 'US']);
    }
  }

  /** evaluates new suggestions based on keyword only */
  private getCities(
    searchString: string,
    minPopulation: number,
    countries: string[],
  ): IGetCitySuggestion[] {
    const filteredCities = this.#citiesData.filter(
      (c) => c.population > minPopulation && countries.includes(c.country),
    );
    const suggestedCities: IGetCitySuggestion[] = scoreByNameSimilarity(filteredCities, searchString);
    return sortByScore(suggestedCities)
  }

  /** gets suggestions based on geolocation provided */
  public getSuggestionsWithCoordinates(
      searchString: string,
      latitude: string,
      longitude: string,
      minPopulation = 5000,
      countries = ['US', 'CA'],
  ): IGetCitySuggestion[] {
    const citySuggestions = this.getCities(searchString, minPopulation, countries);
    return sortByScore(scoreByDistance(citySuggestions, latitude, longitude));
  }

}
