import {
    CACHE_MANAGER,
    Controller,
    Get,
    HttpCode,
    HttpException,
    HttpStatus,
    Inject,
    Logger,
    Query
} from '@nestjs/common';
import {SuggestionService} from '../service/suggestion.service';
import {CityDto} from "../dto/city.dto";
import {Cache} from 'cache-manager';
import {CityQuery} from "../entity/city.query";
import {ApiOperation, ApiTags} from "@nestjs/swagger";

@Controller('/suggestions')
export class SuggestionController {

    private readonly logger = new Logger(SuggestionController.name);

    constructor(private suggestionService: SuggestionService,
                @Inject(CACHE_MANAGER) private cacheManager: Cache) {
    }

    @HttpCode(HttpStatus.OK)
    @ApiTags('suggestions')
    @ApiOperation({description: 'Get cities for suggestions based on query'})
    @Get()
    async getCities(
        @Query() cityQuery?: CityQuery
    ): Promise<any> {
        const keyword = cityQuery.q;
        const latitude = cityQuery.latitude;
        const longitude = cityQuery.longitude;

        const citiesInRedis = await this.cacheManager.get(keyword);
        if (citiesInRedis != null) {
            this.logger.log('Cities stored in cache for keyword: ' + keyword);
            return citiesInRedis;
        }
        let cities: any[];
        this.logger.log('Find cities called with params : keyword : ' + keyword + ', latitude : ' + latitude + ', longitude : ' + longitude);
        cities = await this.suggestionService.findCitiesWithParams(keyword, latitude, longitude).then(value => value);

        // Short TTL - 10 minutes
        await this.cacheManager.set(keyword, cities, {ttl: 600000});
        this.logger.log('Store in Redis, keyword : ' + keyword + ' for value ' + cities);

        return cities;
    }

    @HttpCode(HttpStatus.OK)
    @ApiTags('suggestions')
    @ApiOperation({description: 'Get all cities for suggestions'})
    @Get('/all')
    getAllCities(): Promise<CityDto[]> {
        this.logger.log('Find all cities called');
        return this.suggestionService.findCities();
    }

}
