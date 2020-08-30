import { FuzzyVector, SearchCityFuzzy } from '../../types/Fuzzy';
import { normalizeText } from '../../utils/textUtils';
import { City, SuggestionResult } from '../../types/City';
import CitySearchEngine from './CitySearchEngine';
import haversine from 'haversine-distance';
export default class FuzzyResolver {

    static convertVectorToMap(bananaGram: FuzzyVector[]): Map<string, number> {
        return bananaGram.reduce((map: Map<string, number>, curr: FuzzyVector) => map.set(curr.nGram, curr.count), new Map<string, number>());
    }

    static calculateCosineSimilarity(searchCity: SearchCityFuzzy, city: City): number {
        const sum = city.nGram.reduce((acc: number, currCity: FuzzyVector) => {
            const countGram = searchCity.searchGram.get(currCity.nGram);
            const multiplier = countGram ? countGram : 0;
            return acc + multiplier * currCity.count;
        }, 0);

        const nameScore = sum / (searchCity.magnitude * city.magnitude);

        if (searchCity.latitude && searchCity.longitude) {
            const scoreGeolocation = this.getScoreFromLocation(searchCity.latitude, searchCity.longitude, city);
            return 0.6 * nameScore + 0.4 * scoreGeolocation;
        }
        return nameScore;
    }

    private static getScoreFromLocation(latitude: string, longitude: string, city: City): number {
        const distanceKm: number = haversine({ latitude: Number(latitude), longitude: Number(longitude) }, { latitude: city.latitude, longitude: city.longitude }) / 1000;
        if (distanceKm < 50) {
            return 1.0;
        }
        return distanceKm > 2000 ? 0.0 : 1.0 - (distanceKm / 2000);
    }

    static calculateMagnitude(vector: FuzzyVector[]): number {
        return Math.sqrt(vector.reduce((prev: number, curr: FuzzyVector, idx: number, arr: FuzzyVector[]) =>
            (prev += Math.pow(curr.count, 2))
            , 0));
    }

    static getTriGram(word: string): FuzzyVector[] {
        const chunk = this.splitChunk(word);
        const group = chunk.reduce((prev: any, curr: string, idx: number, array: string[]) => {
            prev[curr] = prev[curr] ? prev[curr] + 1 : 1;
            return prev;
        }, {});
        return Object.keys(group).reduce((prev: FuzzyVector[], curr: string, idx: number, array: string[]) => {
            prev.push({ nGram: curr, count: group[curr] });
            return prev;
        }, []);
    }

    private static splitChunk(word: string): string[] {
        const normalized = `-${normalizeText(word)}-`;
        const result: string[] = [];
        for (let x = 0; x < normalized.length - 2; ++x) {
            result.push(normalized.slice(x, x + 3));
        }
        return result;
    }

}
