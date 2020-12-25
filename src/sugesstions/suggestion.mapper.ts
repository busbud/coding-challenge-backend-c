import { CityQueryResult } from '../cities/interfaces/city-query-result';
import { Suggestion } from './interfaces/suggestion';
import { ScoreCalculator } from './score.calculator';

export class SuggestionMapper {
  constructor(private score: ScoreCalculator) {}

  toSuggestion(city: CityQueryResult): Suggestion & Record<string, any> {
    return {
      name: `${city.name}, ${city.state}, ${city.country}`,
      nativeScore: city.searchScore,
      score: this.score.getScore(city),
      longitude: String(city.location.lat),
      latitude: String(city.location.lng),
      population: city.population,
      normalized_name: city.normalized_name,
    };
  }
}
