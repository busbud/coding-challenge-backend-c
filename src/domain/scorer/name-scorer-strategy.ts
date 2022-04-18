import { City } from '../model/entity/city';
import { CityPresenter } from '../model/value-object/city-presenter';
import { SuggestionSearchCriteria } from '../model/value-object/suggestion-search-criteria';
import { IScorerStrategy } from './scorer-strategy-interface';

export class NameScorerStrategy implements IScorerStrategy {
    public executeStrategy(cities: City[], searchCriteria: SuggestionSearchCriteria): CityPresenter[] {
        const cityPresenters: CityPresenter[] = [];

        for (const city of cities) {
            const score: number = this.determineScore(city, searchCriteria);

            cityPresenters.push(new CityPresenter(city.getCompleteName(), city.latitude, city.longitude, score));
        }

        return cityPresenters;
    }

    public determineScore(city: City, searchCriteria: SuggestionSearchCriteria): number {
        let score = 0;

        if (city.name === searchCriteria.q) {
            score = 10;
        } else {
            if (city.name.startsWith(searchCriteria.q)) {
                score = 7.5;
            } else {
                score = 5;   
            }
        }

        return score;
    }
}