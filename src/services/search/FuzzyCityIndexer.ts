import { City } from '../../types/City';
import FuzzyResolver from './FuzzyResolver';
import { FuzzyVector } from '../../types/Fuzzy';

export default class FuzzyCityIndexer {

    cities: City[];
    dictionary: Map<string, number[]> = new Map<string, number[]>();

    constructor(cities: City[]) {
        this.cities = cities;
    }

    buildIndexes(): void {
        this.cities.forEach((city: City, pos: number) => {
            city.nGram = FuzzyResolver.getTrigram(city.name);
            city.magnitude = FuzzyResolver.calculateMagnitude(city.nGram);
            city.nGram.forEach(gram => {
                let indexCities = this.dictionary.get(gram.nGram);
                if (!indexCities) {
                    indexCities = [];
                    this.dictionary.set(gram.nGram, indexCities);
                }
                indexCities.push(pos);
            })
        });
    }

    findCityByGram(triGramSearched: FuzzyVector[]): City[] {
        const citiesIndexes: number[] = triGramSearched.map((fuzzy: FuzzyVector) => fuzzy.nGram)
            .map((nGram: string) => this.getCitiesIndexFromGram(nGram))
            .reduce((prev: number[], curr: number[]) => prev.concat([...curr]), []);

        return Array.from(new Set(citiesIndexes)).map(index => this.cities[index]);
    }

    private getCitiesIndexFromGram(nGram: string): number[] {
        return this.dictionary.has(nGram) ? this.dictionary.get(nGram) : [];
    }
}