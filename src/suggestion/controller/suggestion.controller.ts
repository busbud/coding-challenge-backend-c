import {Controller, Get, Logger, Query} from '@nestjs/common';
import {SuggestionService} from '../service/suggestion.service';
import {CityDto} from "../dto/city.dto";

@Controller('/suggestions')
export class SuggestionController {

    private readonly logger = new Logger(SuggestionController.name);

    constructor(private suggestionService: SuggestionService) {}

    @Get()
    getCities(
        @Query('q') keyword?: string,
        @Query('latitude') latitude?: string,
        @Query('longitude') longitude?: string
    ): Promise<CityDto[]> {
        this.logger.log('Find cities called with params : keyword : ' + keyword + ', latitude : ' + latitude + ', longitude : ' + longitude);
        return this.suggestionService.findCitiesWithParams(keyword, latitude, longitude);
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
