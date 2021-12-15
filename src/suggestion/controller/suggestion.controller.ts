import {CACHE_MANAGER, Controller, Get, Inject, Logger, Query} from '@nestjs/common';
import {SuggestionService} from '../service/suggestion.service';
import {CityDto} from "../dto/city.dto";
import {Cache} from 'cache-manager';

@Controller('/suggestions')
export class SuggestionController {

    private readonly logger = new Logger(SuggestionController.name);

    constructor(private suggestionService: SuggestionService,
                @Inject(CACHE_MANAGER) private cacheManager: Cache) {
    }

    @Get()
    getCities(
        @Query('q') keyword?: string,
        @Query('latitude') latitude?: string,
        @Query('longitude') longitude?: string
    ): Promise<any> {
        const citiesInRedis = this.cacheManager.get(keyword);
        if (citiesInRedis) {
            this.logger.log('Cities found in cache for keyword ' + keyword);
            return citiesInRedis;
        }

        this.logger.log('Find cities called with params : keyword : ' + keyword + ', latitude : ' + latitude + ', longitude : ' + longitude);
        const promise = this.suggestionService.findCitiesWithParams(keyword, latitude, longitude);
        this.cacheManager.set(keyword, promise, {ttl: 300});
        this.logger.log('Store in Redis, keyword : ' + keyword + ' for value ' + promise);

        return promise;
    }

    @Get('/all')
    getAllCities(
        @Query('q') keyword?: string,
        @Query('latitude') latitude?: string,
        @Query('longitude') longitude?: string
    ): Promise<CityDto[]> {
        this.logger.log('Find all cities called');
        return this.suggestionService.findCities();
    }

}
