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
import { LocationParser } from '../location/location.parser';
import { map, tap, toArray } from 'rxjs/operators';
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
    private locationParser: LocationParser,
  ) {}

  @Get()
  @UseInterceptors(SuggestionsNotFoundInterceptor)
  findAll(
    @Query() { q, longitude, latitude }: SuggestionRequest,
  ): Observable<SuggestionResponse> {
    const location = this.locationParser.parse(longitude, latitude);
    return this.suggestionService.suggest({ query: q, location }).pipe(
      toArray(),
      map((suggestions) => ({ suggestions })),
    );
  }
}
