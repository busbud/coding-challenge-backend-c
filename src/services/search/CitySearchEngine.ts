import { RequestSearchParam, SuggestionResult, City } from '../../types/City';
import FuzzyCityIndexer from './FuzzyCityIndexer';
import FuzzyResolver from './FuzzyResolver';
import { FuzzyVector, SearchCityFuzzy } from '../../types/Fuzzy';

class CitySearchEngine {

    private static _instance: CitySearchEngine;

    private _indexes: FuzzyCityIndexer;
    private _maxResults: number;
    private _minScore: number;

    findBy(params: RequestSearchParam): SuggestionResult[] {
        if (this._indexes === undefined) {
            throw new Error('First initialize the indexer');
        }
        const triGram: FuzzyVector[] = FuzzyResolver.getTrigram(params.q);
        const citiesSimilarity: City[] = this._indexes.findCityByGram(triGram);

        const searchCity: SearchCityFuzzy = {
            latitude: params.latitude,
            longitude: params.longitude,
            searchGram: FuzzyResolver.convertVectorToMap(triGram),
            magnitude: FuzzyResolver.calculateMagnitude(triGram)
        };

        const result: SuggestionResult[] = citiesSimilarity
            .map((city: City) => this.createSuggestion(city, searchCity))
            .filter((city: SuggestionResult) => this.filterMinScore(city));

        result.sort((a: SuggestionResult, b: SuggestionResult) => a.score > b.score ? -1 : 1);
        return this.limitResults(result);
    }

    private filterMinScore(city: SuggestionResult): boolean {
        return city.score > this._minScore;
    }
    private limitResults(result: SuggestionResult[]): SuggestionResult[] | SuggestionResult[] {
        return result.length > this._maxResults ? result.slice(0, this._maxResults) : result;
    }

    private createSuggestion(city: City, searchCity: SearchCityFuzzy): SuggestionResult {
        return ({
            name: `${city.name}, ${city.province}, ${city.country}`,
            score: FuzzyResolver.calculateCosineSimilarity(searchCity, city),
            latitude: city.latitude,
            longitude: city.longitude
        });
    }

    /**
     * Initialize the index, separating the gram and magnitude into each city
     * @param cities Cities to be indexed for search
     * @param maxResults Max results to be listed in the search
     * @param minScore Minimum score to be filtered in the search
     */
    initialize(cities: City[], maxResults = 10, minScore = 0.3): void {
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