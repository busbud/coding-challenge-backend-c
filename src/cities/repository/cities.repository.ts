import { City } from '../interfaces/city';
import { Observable } from 'rxjs';
import { CityQuery } from '../interfaces/city-query';
import { CityQueryResult } from '../interfaces/city-query-result';

export const CITIES_REPOSITORY_INJECTION_TOKEN = 'CITIES_REPOSITORY';

export interface CitiesRepository {
  query(query: CityQuery): Observable<CityQueryResult>;

  remove(id: string): Observable<void>;

  update(city: City): Observable<void>;

  add(city: City): Observable<void>;
}
