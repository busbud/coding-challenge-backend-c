import { Inject, Injectable } from '@nestjs/common';
import { Observable } from 'rxjs';
import { City } from './interfaces/city';
import {
  CITIES_REPOSITORY_INJECTION_TOKEN,
  CitiesRepository,
} from './repository';
import { CityQuery } from './interfaces/city-query';
import { CityQueryResult } from './interfaces/city-query-result';

@Injectable()
export class CitiesService {
  constructor(
    @Inject(CITIES_REPOSITORY_INJECTION_TOKEN)
    private repository: CitiesRepository,
  ) {}

  queryCities(query: CityQuery): Observable<CityQueryResult> {
    return this.repository.query(query);
  }

  addCity(city: City): Observable<void> {
    return this.repository.add(city);
  }

  updateCity(city: City): Observable<void> {
    return this.repository.update(city);
  }

  removeCity(id: string): Observable<void> {
    return this.repository.remove(id);
  }
}
