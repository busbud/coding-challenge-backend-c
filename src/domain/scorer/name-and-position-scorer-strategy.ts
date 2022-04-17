import { City } from '../model/entity/city';
import { CityPresenter } from '../model/value-object/city-presenter';
import { SuggestionSearchCriteria } from '../model/value-object/suggestion-search-criteria';
import { NameScorerStrategy } from './name-scorer-strategy';
import { PositionScorerStrategy } from './position-scorer-strategy';
import { IScorerStrategy } from './scorer-strategy-interface';

export class NameAndPositionScorerStrategy implements IScorerStrategy {
    private nameScorerStrategy: NameScorerStrategy;
    private positionScorerStrategy: PositionScorerStrategy;

    public executeStrategy(cities: City[], searchCriteria: SuggestionSearchCriteria): CityPresenter[] {
        this.nameScorerStrategy = new NameScorerStrategy();
        this.positionScorerStrategy = new PositionScorerStrategy();
        const cityPresenters: CityPresenter[] = [];

        for (const city of cities) {
            const score: number = this.determineScore(city, searchCriteria);

            cityPresenters.push(new CityPresenter(city.name, city.latitude, city.longitude, score));
        }

        return cityPresenters;
    }

    public determineScore(city: City, searchCriteria: SuggestionSearchCriteria): number {
        return Math.floor((
            this.nameScorerStrategy.determineScore(city, searchCriteria) +
            this.positionScorerStrategy.determineScore(city, searchCriteria)
        ) / 2);
    }
}