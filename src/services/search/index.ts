import * as Diacritic from 'diacritic';
import Cities, { Provinces } from '../cities';
import { helpers, notValidNumber } from '../../utils';
import { City } from '../../types/city';
import FuzzySet from 'fuzzyset.js';
import { GlobeLocation } from '../../types/core';
import { SearchParams } from '../../types/search';
import { Suggestion } from '../../types/suggestions';

type FuzzyResult = [number, string];

export default class Search {
  CitiesService: Cities = new Cities();
  cities: City[] = this.CitiesService.createCitiesArray();
  provinces: Provinces = this.CitiesService.createProvinceList();
  dictionary: string[] = this.cities.map((city) => city.name);
  fuzzySet = FuzzySet(this.dictionary, false);

  guardAgainstInvalidSearchParams(params: SearchParams): void {
    const { q, longitude, latitude } = params;
    if (!q) throw new Error('Missing required `q`');
    if (longitude && notValidNumber(longitude)) throw new Error('Invalid `longitude`');
    if (latitude && notValidNumber(latitude)) throw new Error('Invalid `latitude`');
    if ((latitude && !longitude) || (longitude && !latitude))
      throw new Error('Provide both `latitude` and `longitude`');
  }

  search(params: SearchParams): Suggestion[] {
    this.guardAgainstInvalidSearchParams(params);
    const pLocation: GlobeLocation =
      params.longitude && params.latitude
        ? { long: parseFloat(params.longitude), lat: parseFloat(params.latitude) }
        : undefined;
    return this.fuzzySet
      .get(params.q, [], 0.5)
      .map((suggestion) => this.mapResultToSuggestion(pLocation, suggestion))
      .sort((a, b) => b.score - a.score);
  }

  mapResultToSuggestion(location: GlobeLocation, [score, cityName]: FuzzyResult): Suggestion {
    const { name, lat, long, provId, country }: City = this.cities.find((c) => c.name === cityName);
    const fullCityName = `${name}, ${this.provinces[`${country}.${provId}`]}, ${country}`;

    return {
      name: Diacritic.clean(fullCityName),
      latitude: lat,
      longitude: long,
      score: location ? (score + helpers.DistanceHelper.getGeoScore(location, { long, lat })) / 2 : score,
    };
  }
}
