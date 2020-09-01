import { FuzzyVector, SearchCityFuzzy } from '../../types/Fuzzy';
import { chunk } from '../../services/textUtils';
import { City } from '../../types/City';
import haversine from 'haversine-distance';
import { GEOLOCATION_MIN_KM, GEOLOCATION_MAX_KM } from '../../constants/fuzzyConstants';

export default class FuzzyResolver {

    static convertVectorToMap(nGramVector: FuzzyVector[]): Map<string, number> {
        return nGramVector.reduce((map: Map<string, number>, curr: FuzzyVector) => map.set(curr.nGram, curr.count), new Map<string, number>());
    }

    static calculateCosineSimilarity(searchCity: SearchCityFuzzy, city: City): number {
        const sum = city.nGram.reduce((accumulator: number, currCity: FuzzyVector) => {
            const countGram = searchCity.searchGram.get(currCity.nGram);
            const multiplier = countGram ? countGram : 0;
            return accumulator + multiplier * currCity.count;
        }, 0);

        const nameScore = sum / (searchCity.magnitude * city.magnitude);

        if (searchCity.latitude && searchCity.longitude) {
            const scoreGeolocation = this.getScoreFromLocation(searchCity.latitude, searchCity.longitude, city);
            return 0.7 * nameScore + 0.3 * scoreGeolocation;
        }
        return nameScore;
    }

    static calculateMagnitude(nGramVector: FuzzyVector[]): number {
        return Math.sqrt(nGramVector.reduce((sum: number, currNGram: FuzzyVector) =>
            (sum + Math.pow(currNGram.count, 2))
            , 0));
    }

    static getTrigram(word: string): FuzzyVector[] {
        const chunkVector = chunk(word);
        const dictCountNGram = this.countNGramRepeated(chunkVector);
        return this.convertDictionaryToFuzzyVector(dictCountNGram);
    }

    private static getScoreFromLocation(latitude: string, longitude: string, city: City): number {
        const distanceKm: number = haversine({ latitude: Number(latitude), longitude: Number(longitude) }, { latitude: city.latitude, longitude: city.longitude }) / 1000;
        if (distanceKm < GEOLOCATION_MIN_KM) {
            return 1.0;
        }
        return distanceKm > GEOLOCATION_MAX_KM ? 0.0 : 1.0 - (distanceKm / GEOLOCATION_MAX_KM);
    }

    private static convertDictionaryToFuzzyVector(dictCountNGram: any): FuzzyVector[] {
        return Object.keys(dictCountNGram).reduce((prev: FuzzyVector[], curr: string) => {
            prev.push({ nGram: curr, count: dictCountNGram[curr] });
            return prev;
        }, []);
    }

    private static countNGramRepeated(chunkVector: string[]) {
        return chunkVector.reduce((prev: any, curr: string) => {
            prev[curr] = prev[curr] ? prev[curr] + 1 : 1;
            return prev;
        }, {});
    }



}
