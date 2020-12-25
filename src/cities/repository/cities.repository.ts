import { Observable } from 'rxjs';
import { CityQuery } from '../interfaces/city-query';
import { CityQueryResult } from '../interfaces/city-query-result';

export const CITIES_REPOSITORY_INJECTION_TOKEN = 'CITIES_REPOSITORY';

export interface CitiesRepository {
  query(query: CityQuery): Observable<CityQueryResult>;

  getMaxPopulation(): Observable<number>;
}
