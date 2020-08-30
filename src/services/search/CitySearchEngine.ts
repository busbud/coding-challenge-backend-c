import { RequestSearchParam, SuggestionResult, CityParam, City } from '../../types/City';
import FuzzyCityIndexer from './FuzzyCityIndexer';
import FuzzyResolver from './FuzzyResolver';
import { FuzzyVector } from '../../types/Fuzzy';

class CitySearchEngine {

    private static _instance: CitySearchEngine;
    private _indexes: FuzzyCityIndexer;
    private _maxResults: number;
    private _minScore: number;

    findBy(params: RequestSearchParam): Promise<SuggestionResult[]> {
        if (this._indexes === undefined) {
            throw new Error('First initialize the indexer');
        }
        return new Promise((resolve, reject) => {
            const triGram: FuzzyVector[] = FuzzyResolver.getTriGram(params.q);
            const magnitude: number = FuzzyResolver.calculateMagnitude(triGram);

            const citiesSimilarity: City[] = this._indexes.findCityByGram(triGram);
            const searchMapGram: Map<string, number> = FuzzyResolver.convertVectorToMap(triGram);

            const result = citiesSimilarity.map((city: City) => ({
                name: `${city.name}, ${city.province}, ${city.country}`,
                score: FuzzyResolver.calculateCosineSimilarity(searchMapGram, magnitude, city),
                latitude: city.latitude,
                longitude: city.longitude
            })).filter((city: SuggestionResult) => city.score > this._minScore);

            result.sort((a: SuggestionResult, b: SuggestionResult) => a.score > b.score ? -1 : 1);
            resolve(result.length > this._maxResults ? result.slice(0, this._maxResults) : result);
        });
    }

    /**
     * Initialize the index, separating the gram and magnitude into each city
     * @param cities Cities to be indexed for search
     * @param maxResults Max results to be listed in the search
     * @param minScore Minimum score to be filtered in the search
     */
    initialize(cities: City[], maxResults: number = 10, minScore: number = 0.3) {
        this._maxResults = maxResults;
        this._minScore = minScore;
        this._indexes = new FuzzyCityIndexer(cities);
        this._indexes.buildIndexes();
    }

    get cities(): City[] {
        return this._indexes.cities;
    }

    public static get instance(): CitySearchEngine {
        if (!CitySearchEngine._instance) {
            CitySearchEngine._instance = new CitySearchEngine();
        }
        return CitySearchEngine._instance;
    }
}

export default CitySearchEngine;