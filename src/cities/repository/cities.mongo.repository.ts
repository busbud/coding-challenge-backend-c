import {
  Inject,
  Injectable,
  Logger,
  OnApplicationBootstrap,
} from '@nestjs/common';
import { defer, from, Observable, of } from 'rxjs';
import { CityQuery } from '../interfaces/city-query';
import { CitiesRepository } from './cities.repository';
import { CityQueryResult } from '../interfaces/city-query-result';
import { MongoInitializerService } from './mongo-initializer.service';
import { InjectModel } from '@nestjs/mongoose';
import { Document, Model, Query } from 'mongoose';
import { CityDocument } from './city.schema';
import { switchMap, takeWhile, tap } from 'rxjs/operators';
import { City } from '../interfaces/city';
import {
  CITIES_MONGO_CONFIGURATION,
  CitiesMongoRepositoryConfiguration,
} from './config';

declare module 'mongoose' {
  export interface Model<T extends Document> {
    fuzzySearch: <R = T>(string, query?: any) => Query<Array<R>, T>;
  }
}

@Injectable()
export class CitiesMongoRepository
  implements CitiesRepository, OnApplicationBootstrap {
  private readonly logger = new Logger(CitiesMongoRepository.name);

  constructor(
    private readonly initializer: MongoInitializerService,
    @InjectModel('City') private readonly cityModel: Model<CityDocument>,
    @Inject(CITIES_MONGO_CONFIGURATION)
    private config: CitiesMongoRepositoryConfiguration,
  ) {}

  query({ query, limit }: CityQuery): Observable<CityQueryResult> {
    const state = this.initializer.state;
    if (!state.ready) {
      this.logger.warn(
        `Attempted to query cities before cache was ready: ${state.notReadyMessage}`,
      );
      return from([]);
    }
    const scoreThreshold = query.length * this.config.scoreThreshold;

    const cities$ = defer(() =>
      this.cityModel
        .fuzzySearch<City & { searchScore: number }>(query)
        .select({
          id: 1,
          geohash: 1,
          name: 1,
          alt_name: 1,
          normalized_name: 1,
          location: 1,
          population: 1,
          country: 1,
          state: 1,
          searchScore: { $meta: 'textScore' },
        })
        .lean()
        .limit(limit || 100)
        .exec(),
    );
    return cities$.pipe(
      tap({
        next: (res) =>
          this.logger.log(`Successfully fetched ${res.length} cities`),
        error: (err) =>
          this.logger.error(`Error fetching cities ${err.message}`),
      }),
      switchMap((results) => {
        const scoreDivider = results[0]?.searchScore;
        return from(results).pipe(
          takeWhile((r) => r.searchScore > scoreThreshold),
          tap((r) => {
            r.searchScore /= scoreDivider;
          }),
        );
      }),
    );
  }

  getMaxPopulation(): Observable<number> {
    return of(this.initializer.state.maxPopulation);
  }

  async onApplicationBootstrap() {
    await this.initializer.init();
  }
}
