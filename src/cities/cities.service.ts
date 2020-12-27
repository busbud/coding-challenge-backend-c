import { Inject, Injectable, Logger } from '@nestjs/common';
import { Observable } from 'rxjs';
import {
  CITIES_REPOSITORY_INJECTION_TOKEN,
  CitiesRepository,
} from './repository';
import { CityQuery } from './interfaces/city-query';
import { CityQueryResult } from './interfaces/city-query-result';

@Injectable()
export class CitiesService {
  private readonly logger = new Logger(CitiesService.name);
  constructor(
    @Inject(CITIES_REPOSITORY_INJECTION_TOKEN)
    private repository: CitiesRepository,
  ) {}

  queryCities(query: CityQuery): Observable<CityQueryResult> {
    this.logger.log(`Resolving cities for: ${query.query}`);
    return this.repository.query(query);
  }

  getMaxPopulation(): Observable<number> {
    return this.repository.getMaxPopulation();
  }
}
