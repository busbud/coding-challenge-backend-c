import {
  CallHandler,
  Controller,
  ExecutionContext,
  Get,
  NestInterceptor,
  NotFoundException,
  Query,
  UseInterceptors,
} from '@nestjs/common';
import { SuggestionsService } from './suggestions.service';
import { Observable } from 'rxjs';
import { LocationService } from '../location/location.service';
import { map, tap } from 'rxjs/operators';
import { SuggestionRequest } from './interfaces/suggestion-request';
import { SuggestionResponse } from './interfaces/suggestion-response';

export class SuggestionsNotFoundInterceptor implements NestInterceptor {
  intercept(context: ExecutionContext, next: CallHandler): Observable<any> {
    return next.handle().pipe(
      tap((data) => {
        if (!data.suggestions.length) {
          throw new NotFoundException({ suggestions: [] });
        }
      }),
    );
  }
}

@Controller('suggestions')
export class SuggestionsController {
  constructor(
    private suggestionService: SuggestionsService,
    private locationParser: LocationService,
  ) {}

  @Get()
  @UseInterceptors(SuggestionsNotFoundInterceptor)
  findAll(
    @Query() { q, longitude, latitude, limit }: SuggestionRequest,
  ): Observable<SuggestionResponse> {
    const location = this.locationParser.parse(longitude, latitude);
    return this.suggestionService
      .suggest({ query: q, location, limit: limit || 5 })
      .pipe(map((suggestions) => ({ suggestions })));
  }
}
