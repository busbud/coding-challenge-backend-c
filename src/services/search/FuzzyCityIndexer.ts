import { City } from '../../types/City';
import FuzzyResolver from './FuzzyResolver';
import { FuzzyVector } from '../../types/Fuzzy';

export default class FuzzyCityIndexer {

    cities: City[];
    dictionary: Map<string, number[]> = new Map<string, number[]>();

    constructor(cities: City[]) {
        this.cities = cities;
    }

    buildIndexes() {
        this.cities.forEach((city: City, pos: number) => {
            city.nGram = FuzzyResolver.getTriGram(city.name);
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

    findCityByGram(gram: FuzzyVector[]): City[] {
        const citiesIndexes: number[] = gram.map((fuzzy: FuzzyVector) => fuzzy.nGram)
            .map((nGram: string) => this.dictionary.has(nGram) ? this.dictionary.get(nGram) : [])
            .reduce((prev: number[], curr: number[]) => prev.concat([...curr]), []);

        const set = new Set(citiesIndexes)
        const citiesFiltered: City[] = [];

        set.forEach((index: number) => citiesFiltered.push(this.cities[index]));

        return citiesFiltered;
    }
}