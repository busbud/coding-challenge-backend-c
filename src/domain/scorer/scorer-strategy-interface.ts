import { City } from '../model/entity/city';
import { CityPresenter } from '../model/value-object/city-presenter';
import { SuggestionSearchCriteria } from '../model/value-object/suggestion-search-criteria';

export interface IScorerStrategy {
    executeStrategy(cities: City[], searchCriteria: SuggestionSearchCriteria): CityPresenter[];
    determineScore(city: City, searchCriteria: SuggestionSearchCriteria): number;
}