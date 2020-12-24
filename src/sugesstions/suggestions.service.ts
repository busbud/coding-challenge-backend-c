import { Injectable } from '@nestjs/common';
import { Observable } from 'rxjs';
import { Suggestion } from './interfaces/suggestion';
import { CitiesService } from '../cities/cities.service';
import { map, take } from 'rxjs/operators';
import { SuggestionQuery } from './interfaces/suggestion-query';
import { CityQueryResult } from '../cities/interfaces/city-query-result';

@Injectable()
export class SuggestionsService {
  constructor(private cities: CitiesService) {}

  suggest({ query }: SuggestionQuery): Observable<Suggestion> {
    const mapper = new SuggestionMapper();
    return this.cities.queryCities({ query }).pipe(
      map((city) => mapper.toSuggestion(city)),
      take(10),
    );
  }
}

class SuggestionMapper {
  toSuggestion(city: CityQueryResult): Suggestion {
    return {
      name: city.name,
      score: city.score,
      longitude: String(city.location.lat),
      latitude: String(city.location.lng),
    };
  }
}
