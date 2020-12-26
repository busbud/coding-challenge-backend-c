import { CityQueryResult } from '../cities';
import { Suggestion } from './interfaces/suggestion';
import { IScoreCalculator } from './score.calculator';
import { v4 as uuidv4 } from 'uuid';

export class SuggestionMapper {
  constructor(private score: IScoreCalculator) {}

  toSuggestion(city: CityQueryResult): Suggestion {
    return {
      id: uuidv4(),
      name: `${city.name}, ${city.state}, ${city.country}`,
      score: this.score.getScore(city),
      longitude: String(city.location.lng),
      latitude: String(city.location.lat),
    };
  }
}
