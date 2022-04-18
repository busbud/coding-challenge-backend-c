import { City } from '../model/entity/city';
import { CityPresenter } from '../model/value-object/city-presenter';
import { SuggestionSearchCriteria } from '../model/value-object/suggestion-search-criteria';
import { IScorerStrategy } from './scorer-strategy-interface';

export class PositionScorerStrategy implements IScorerStrategy {
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

        if (city.latitude === searchCriteria.latitude && city.longitude === searchCriteria.longitude) {
            score = 10;
        } else {
            const distanceBetweenCityAndSearchCriteria: number = this.calculateDistance(city, searchCriteria);

            if (distanceBetweenCityAndSearchCriteria <= 10) {
                score = 9;
            } else if (distanceBetweenCityAndSearchCriteria <= 50) {
                score = 8;
            } else if (distanceBetweenCityAndSearchCriteria <= 250) {
                score = 7;
            } else if (distanceBetweenCityAndSearchCriteria <= 500) {
                score = 6;
            } else if (distanceBetweenCityAndSearchCriteria <= 1000) {
                score = 5;
            } else if (distanceBetweenCityAndSearchCriteria <= 2000) {
                score = 4;
            } else if (distanceBetweenCityAndSearchCriteria <= 4000) {
                score = 3;
            } else if (distanceBetweenCityAndSearchCriteria <= 8000) {
                score = 2;
            } else {
                score = 1;
            }
        }

        return score;
    }

    private calculateDistance(city: City, searchCriteria: SuggestionSearchCriteria): number {
        const radlat1: number = Math.PI * (city.latitude / 180);
        const radlat2: number = Math.PI * (searchCriteria.latitude / 180);
        const theta: number = city.longitude - searchCriteria.longitude;

        const radtheta: number = Math.PI * theta/180;

        let distance: number = Math.sin(radlat1) * Math.sin(radlat2) + Math.cos(radlat1) * Math.cos(radlat2) * Math.cos(radtheta);

        distance = Math.acos(distance);
        distance = distance * (180 / Math.PI);
        distance = distance * 60 * 1.1515;
        distance = distance * 1.609344;

        return distance;
    }
}