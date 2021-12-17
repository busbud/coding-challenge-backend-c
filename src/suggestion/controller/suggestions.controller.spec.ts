import { SuggestionController } from './suggestion.controller';
import { SuggestionService } from './../service/suggestion.service';
import {Cache} from 'cache-manager';
import {CityEntity} from '../../city/city.entity';
import {Repository} from 'typeorm';
import {CityService} from "../../city/city.service";
import {SuggestionsDto} from "../dto/suggestions.dto";

describe('SuggestionController', () => {
    let suggestionController: SuggestionController;
    let suggestionService: SuggestionService;
    let cityService: CityService;
    let repository: Repository<CityEntity>;
    let cache = Cache;

    beforeEach(() => {
        repository = new Repository<CityEntity>();
        cache = new Cache();
        suggestionService = new SuggestionService(repository, cityService, cache);
        suggestionController = new SuggestionController(suggestionService, cache);
    });

    describe('findSuggestionsWithParams', () => {
        it('should return an array of cats', async () => {
            const result = new SuggestionsDto();
            expect(await suggestionService.findSuggestionsWithParams("fdsfs", "", "")).toBe(result);
        });
    });
});
