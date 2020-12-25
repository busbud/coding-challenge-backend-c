import { Inject, Injectable } from '@nestjs/common';
import { Observable } from 'rxjs';
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

  getMaxPopulation(): Observable<number> {
    return this.repository.getMaxPopulation();
  }
}
