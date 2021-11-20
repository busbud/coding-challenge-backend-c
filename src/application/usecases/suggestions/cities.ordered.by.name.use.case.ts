import { ICity } from '@domain/interfaces/i.city';
import { ICitySuggestions } from '@domain/interfaces/suggestions/i.city.suggestions';
import { Injectable } from '@nestjs/common';
const stringSimilarity = require("string-similarity");
@Injectable()
export class CitiesOrderedByNameUseCase {
  async execute(suggestions: ICity[], cityName: string): Promise<ICitySuggestions[]> {
    const citiesWithTheirSimilaties = await this.getCitiesSimilaties(suggestions, cityName);

   const sortedCities = this.sortCitiesByScore(citiesWithTheirSimilaties);

    return sortedCities;
  }
  private async getCitiesSimilaties(suggestions: ICity[], cityName :string): Promise<ICitySuggestions[]> {
    const result = suggestions.map((suggestion) => {
      const { name, lat, long, country } = suggestion;

      const similarity = stringSimilarity.compareTwoStrings(suggestion.name, cityName);

      const citySuggestions: ICitySuggestions = { ...{ name: `${name}, ${country}`, latitude: lat, longitude: long }, score:  similarity.toFixed(1) };

      return citySuggestions;
    });
    return result;
  }

  private sortCitiesByScore(citiesSuggestions: ICitySuggestions[]) {
 
    citiesSuggestions.sort((a, b) => parseFloat(b.score) - parseFloat(a.score));

    return citiesSuggestions;
  }
}
