import { FuzzyVector } from '../../types/Fuzzy';
import { normalizeText } from '../../utils/textUtils';
import { City } from '../../types/City';

export default class FuzzyResolver {

    static convertVectorToMap(bananaGram: FuzzyVector[]): Map<string, number> {
        return bananaGram.reduce((map: Map<string, number>, curr: FuzzyVector) => map.set(curr.nGram, curr.count), new Map<string, number>());
    }

    static calculateCosineSimilarity(searchGram: Map<string, number>, magnitude: number, city: City): number {
        const sum = city.nGram.reduce((acc: number, currCity: FuzzyVector) => {
            const countGram = searchGram.get(currCity.nGram);
            const multiplier = countGram ? countGram : 0;
            return acc + multiplier * currCity.count;
        }, 0);
        return sum / (magnitude * city.magnitude);
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
