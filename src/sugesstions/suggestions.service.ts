import { Inject, Injectable } from '@nestjs/common';
import { Observable, OperatorFunction } from 'rxjs';
import { Suggestion } from './interfaces/suggestion';
import { CitiesService } from '../cities/cities.service';
import { map, mergeMap, tap, toArray } from 'rxjs/operators';
import { SuggestionQuery } from './interfaces/suggestion-query';
import { SuggestionMapper } from './suggestion.mapper';
import { ScoreCalculator } from './score.calculator';
import { SuggestionsSorter } from './suggestion.sorter';
import { EventEmitter2 } from '@nestjs/event-emitter';
import { SuggestionsEvents } from '../app-events';
import { LocationService } from '../location/location.service';

export const SUGGESTIONS_CONFIG_INJECTION_TOKEN = 'SUGGESTIONS_CONFIGURATION';

export type SuggestionsServiceConfiguration = {
  reportReturned: boolean;
};

@Injectable()
export class SuggestionsService {
  constructor(
    private cities: CitiesService,
    private locationService: LocationService,
    private events: EventEmitter2,
    @Inject(SUGGESTIONS_CONFIG_INJECTION_TOKEN)
    private config: SuggestionsServiceConfiguration,
  ) {}

  suggest(options: SuggestionQuery): Observable<Suggestion[]> {
    const { query, limit } = options;
    return this.prepareSuggestionMapper(options).pipe(
      mergeMap((mapper) =>
        this.cities.queryCities({ query }).pipe(
          map((city) => [mapper.toSuggestion(city), city]),
          tap(([suggestion, city]) =>
            this.events.emit(
              SuggestionsEvents.SUGGESTION_GENERATED,
              suggestion,
              city,
              options,
            ),
          ),
          map(([suggestion]) => suggestion as Suggestion),
        ),
      ),
      this.sort(),
      map((results) => results.slice(0, limit)),
      this.reportReturned(),
    );
  }

  private prepareSuggestionMapper({
    query,
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
                query,
                maxDistance: this.locationService.nearByDistanceDivider,
              },
              {
                weights: {
                  population: 0.3,
                  criteria: 0.6,
                  nearBy: 0.1,
                },
              },
            ),
          ),
      ),
    );
  }

  private sort(): OperatorFunction<Suggestion, Suggestion[]> {
    return (suggestions) =>
      suggestions.pipe(
        toArray(),
        map((suggestions) => suggestions.sort(SuggestionsSorter())),
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
