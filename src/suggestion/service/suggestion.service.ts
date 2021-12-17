import {CACHE_MANAGER, Inject, Injectable, Logger} from '@nestjs/common';
import {InjectRepository} from '@nestjs/typeorm';
import {Repository} from 'typeorm';
import {Cache} from 'cache-manager';
import {SuggestionDto} from '../dto/suggestion.dto';
import {CityService} from '../../city/city.service';
import {CityDto} from './../dto/city.dto';
import {CityEntity} from '../../city/city.entity';
import {SuggestionsDto} from '../dto/suggestions.dto';

@Injectable()
export class SuggestionService {
  private readonly logger = new Logger(SuggestionService.name);

  constructor(
        @InjectRepository(CityEntity)
        private cityRepository: Repository<CityEntity>,
        private cityService: CityService,
        @Inject(CACHE_MANAGER) private cacheManager: Cache,
  ) {
  }

  /**
     * Format a city to a suggestion
     * @param city
     * @private
     */
  private static formatCityToSuggestion(city: CityDto): SuggestionDto {
    return {
      name: city.name + ', ' + city.admin1 + ', ' + city.country,
      latitude: city.lat,
      longitude: city.long,
      score: Math.round((city.score || 0) * 100) / 100,
    };
  }

  /**
     *
     * Retrieve suggestions in DB based in params, returns array of suggestions
     * @param q
     * @param latitude
     * @param longitude
     */
  async findSuggestionsWithParams(q: string, latitude: string, longitude: string): Promise<SuggestionsDto> {
    this.logger.log('Find suggestions with params : keyword : ' + q + ', latitude : ' + latitude + ', longitude : ' + longitude);

    // If the query is only based on name, we are searching suggestions into redis cache first
    if (!latitude || !longitude) {
      const suggestionsInCache = await this.cacheManager.get(q);
      if (suggestionsInCache != null) {
        this.logger.log('Suggestions stored in cache for keyword: ' + q);
        return suggestionsInCache;
      }
    }

    this.logger.log('Find suggestions called with params : keyword : ' + q + ', latitude : ' + latitude + ', longitude : ' + longitude);
    const suggestionDtos = (await this.cityService.findCitiesWithParams(q, latitude, longitude)).map(SuggestionService.formatCityToSuggestion);

    // Store suggestions into Redis only for name query, we don't want to store it for coordinates
    if (suggestionDtos.length && (!latitude || !longitude)) {
      await this.cacheManager.set(q, suggestionDtos);
      this.logger.log('Store suggestions in Redis, keyword : ' + q);
    }

    // SuggestionsDto object to return result
    const suggestions = new SuggestionsDto();
    suggestions.suggestions = suggestionDtos;

    return suggestions;
  }
}
