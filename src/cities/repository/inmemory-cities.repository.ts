import { Injectable, OnModuleInit } from '@nestjs/common';
import Fuse from 'fuse.js';
import { City } from '../interfaces/city';
import { CitiesSeeder } from './cities.seeder';
import { from, Observable, of } from 'rxjs';
import { map, mergeMap, tap, toArray } from 'rxjs/operators';
import { CityQuery } from '../interfaces/city-query';
import { CitiesRepository } from './cities.repository';
import { CityQueryResult } from '../interfaces/city-query-result';
import { EventEmitter2 } from '@nestjs/event-emitter';

@Injectable()
export class CitiesInMemoryRepository
  implements CitiesRepository, OnModuleInit {
  private fuse: Fuse<City>;

  constructor(private seeder: CitiesSeeder, private events: EventEmitter2) {
    this.fuse = new Fuse<City>([]);
  }

  private loadCities(): Observable<City[]> {
    return this.seeder.loadCities().pipe(
      toArray(),
      tap((cities) => this.initializeFuse(cities)),
    );
  }

  private initializeFuse(cities: City[]) {
    const options = {
      includeScore: true,
      keys: [
        {
          name: 'name',
          weight: 0.7,
        },
        {
          name: 'alt_name',
          weight: 0.3,
        },
      ],
    };
    const index = Fuse.createIndex(options.keys, cities);
    this.fuse = new Fuse<City>(cities, options, index);
  }

  onModuleInit(): any {
    this.loadCities().subscribe(() => this.events.emit('cities.ready', true));
  }

  query({ query }: CityQuery): Observable<CityQueryResult> {
    return from(this.fuse.search(query)).pipe(
      map(({ item, score }) => ({
        ...item,
        score: score || 0,
      })),
    );
  }

  add(city: City): Observable<void> {
    return of(void 0).pipe(tap(() => this.fuse.add(city)));
  }

  update(city: City): Observable<void> {
    return this.remove(city.id).pipe(mergeMap(() => this.add(city)));
  }

  remove(id: string): Observable<void> {
    return of(void 0).pipe(tap(() => this.fuse.remove((it) => it.id === id)));
  }
}
