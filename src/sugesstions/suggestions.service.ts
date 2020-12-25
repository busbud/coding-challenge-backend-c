import { Injectable } from '@nestjs/common';
import { Observable, OperatorFunction } from 'rxjs';
import { Suggestion } from './interfaces/suggestion';
import { CitiesService } from '../cities/cities.service';
import { map, mergeMap, toArray } from 'rxjs/operators';
import { SuggestionQuery } from './interfaces/suggestion-query';
import { SuggestionMapper } from './suggestion.mapper';
import { ScoreCalculator } from './score.calculator';
import { SuggestionsSorter } from './suggestion.sorter';

@Injectable()
export class SuggestionsService {
  constructor(private cities: CitiesService) {}

  suggest({ query }: SuggestionQuery): Observable<Suggestion> {
    return this.prepareSuggestionMapper().pipe(
      mergeMap((mapper) =>
        this.cities
          .queryCities({ query })
          .pipe(map((city) => mapper.toSuggestion(city))),
      ),
    );
  }

  private prepareSuggestionMapper(): Observable<SuggestionMapper> {
    return this.cities
      .getMaxPopulation()
      .pipe(
        map(
          (maxPopulation) =>
            new SuggestionMapper(new ScoreCalculator({ maxPopulation })),
        ),
      );
  }

  sort(): OperatorFunction<Suggestion, Suggestion[]> {
    return (suggestions) =>
      suggestions.pipe(
        toArray(),
        map((suggestions) => suggestions.sort(SuggestionsSorter())),
      );
  }
}
