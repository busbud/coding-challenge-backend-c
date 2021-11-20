import { ICity } from '@domain/interfaces/i.city';
import { ICitySuggestions } from '@domain/interfaces/suggestions/i.city.suggestions';
import { LocationSharedService } from '@domain/shared/services/location.shared.service';
import { Injectable } from '@nestjs/common';

@Injectable()
export class CitiesOrderedByLocUseCase {
  constructor(private readonly locationSharedService: LocationSharedService) {}
  async execute(suggestions, latitude, longitude): Promise<ICitySuggestions[]> {
    const citiesWithTheirDistances = await this.getCitiesDistances(suggestions, latitude, longitude);
    const furthestCity: ICitySuggestions = citiesWithTheirDistances.reduce((prev: any, city) => (prev = prev.distance > city.distance ? prev : city), 0);

    const sortedCities = this.sortCitiesByScore(citiesWithTheirDistances, furthestCity);

    return sortedCities;
  }
  private async getCitiesDistances(suggestions: ICity[], qLatitude: string, qLongitude: string): Promise<ICitySuggestions[]> {
    const result = suggestions.map((suggestion) => {
      const distance = this.locationSharedService.getHowFarCityIs(qLatitude, qLongitude, suggestion.lat, suggestion.long, 'K');

      const { name, lat, long, country } = suggestion;

      const citySuggestions: ICitySuggestions = { ...{ name: `${name}, ${country}`, latitude: lat, longitude: long }, distance };

      return citySuggestions;
    });
    return result;
  }

  private sortCitiesByScore(citiesSuggestions: ICitySuggestions[], furthestCity: ICitySuggestions) {
    const citiesScores = citiesSuggestions.map((city) => {
      const score = 1 - city.distance / furthestCity.distance;
      return { ...city, distance: undefined, score: score.toFixed(1) };
    });

    citiesScores.sort((a, b) => parseFloat(b.score) - parseFloat(a.score));

    return citiesScores;
  }
}
