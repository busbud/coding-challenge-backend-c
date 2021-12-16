import {CACHE_MANAGER, Controller, Get, HttpCode, HttpStatus, Inject, Logger, Query} from '@nestjs/common';
import {ApiOperation, ApiTags} from "@nestjs/swagger";
import {Cache} from 'cache-manager';
import {SuggestionQuery} from "../entity/suggestion.query";
import {SuggestionService} from '../service/suggestion.service';
import {SuggestionsDto} from "../dto/suggestions.dto";

@Controller('/suggestions')
export class SuggestionController {

    private readonly logger = new Logger(SuggestionController.name);

    constructor(private suggestionService: SuggestionService,
                @Inject(CACHE_MANAGER) private cacheManager: Cache) {
    }

    @HttpCode(HttpStatus.OK)
    @ApiTags('suggestions')
    @ApiOperation({description: 'Get suggestions based on query'})
    @Get()
    async getSuggestions(
        @Query() suggestionQuery?: SuggestionQuery
    ): Promise<SuggestionsDto> {
        this.logger.log('Get suggestions called');
        const {q, latitude, longitude} = suggestionQuery;
        return await this.suggestionService.findSuggestionsWithParams(q, latitude, longitude).then(value => value);
    }

}
