import { Inject, Injectable, Logger } from '@nestjs/common';
import { Observable, OperatorFunction } from 'rxjs';
import { Suggestion } from './interfaces/suggestion';
import { CitiesService } from '../cities/cities.service';
import { map, mergeMap, tap } from 'rxjs/operators';
import { SuggestionQuery } from './interfaces/suggestion-query';
import { SuggestionMapper } from './suggestion.mapper';
import { ScoreCalculator } from './score.calculator';
import { EventEmitter2 } from '@nestjs/event-emitter';
import { SuggestionsEvents } from '../app-events';
import { LocationService } from '../location/location.service';
import { TopSuggestionsReducer } from './top-suggestions.reducer';
import { CityQueryResult } from '../cities';

export const SUGGESTIONS_CONFIG_INJECTION_TOKEN = 'SUGGESTIONS_CONFIGURATION';

export type SuggestionsServiceConfiguration = {
  reportReturned: boolean;
  weights: {
    population: number;
    criteria: number;
    nearBy: number;
  };
};

@Injectable()
export class SuggestionsService {
  private readonly logger = new Logger(SuggestionsService.name);
  constructor(
    private cities: CitiesService,
    private locationService: LocationService,
    private events: EventEmitter2,
    @Inject(SUGGESTIONS_CONFIG_INJECTION_TOKEN)
    private config: SuggestionsServiceConfiguration,
  ) {}

  suggest(options: SuggestionQuery): Observable<Suggestion[]> {
    const { query, limit, location } = options;
    this.logger.log(
      `Suggesting cities for: ${query} with limit ${limit} ${
        location ? 'with' : 'without'
      } location`,
    );
    return this.prepareSuggestionMapper(options).pipe(
      mergeMap((mapper) =>
        this.cities.queryCities({ query }).pipe(
          map((city) => [mapper.toSuggestion(city), city]),
          this.reportGenerated(options),
          map(([suggestion]) => suggestion as Suggestion),
        ),
      ),
      TopSuggestionsReducer.keepTopSuggestions(limit),
      this.reportReturned(),
    );
  }

  private prepareSuggestionMapper({
    location,
  }: SuggestionQuery): Observable<SuggestionMapper> {
    return this.cities.getMaxPopulation().pipe(
      map(
        (maxPopulation) =>
          new SuggestionMapper(
            new ScoreCalculator(
              {
                maxPopulation,
                callerLocation: location,
                callerGeohash:
                  location && this.locationService.geohashEncode(location),
                maxDistance: this.locationService.nearByDistanceDivider,
              },
              {
                weights: this.config.weights,
              },
            ),
          ),
      ),
    );
  }

  private reportGenerated(
    query: SuggestionQuery,
  ): OperatorFunction<
    [Suggestion, CityQueryResult],
    [Suggestion, CityQueryResult]
  > {
    return (s$) =>
      s$.pipe(
        tap(([suggestion, city]) =>
          this.events.emit(
            SuggestionsEvents.SUGGESTION_GENERATED,
            suggestion,
            city,
            query,
          ),
        ),
      );
  }

  private reportReturned(): OperatorFunction<Suggestion[], Suggestion[]> {
    if (!this.config.reportReturned) {
      return (s$) => s$;
    }
    return (s$) =>
      s$.pipe(
        tap((suggestions) =>
          suggestions.forEach((s) =>
            this.events.emit(SuggestionsEvents.SUGGESTION_RETURNED, s),
          ),
        ),
      );
  }
}
