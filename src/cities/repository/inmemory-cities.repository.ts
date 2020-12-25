import { Injectable } from '@nestjs/common';
import Fuse from 'fuse.js';
import { City } from '../interfaces/city';
import { from, fromEvent, Observable, of, Subject } from 'rxjs';
import { filter, map } from 'rxjs/operators';
import { CityQuery } from '../interfaces/city-query';
import { CitiesRepository } from './cities.repository';
import { CityQueryResult } from '../interfaces/city-query-result';
import { EventEmitter2 } from '@nestjs/event-emitter';
import {
  CitiesRepositoryEvents,
  CitiesSeederEvents,
  IndexesEvents,
} from '../../app-events';
import { WeightedIndex } from '../../indexes';

@Injectable()
export class CitiesInMemoryRepository implements CitiesRepository {
  private cacheState = new CacheState();
  private fuse: Fuse<City>;

  constructor(private events: EventEmitter2) {
    this.listenEvents();
  }

  private listenEvents() {
    this.cacheState.doIndex$.subscribe(() => this.index());

    fromEvent<City>(
      this.events,
      CitiesSeederEvents.NEW_CITY,
    ).subscribe((city) => this.cacheState.onNewCity(city));

    fromEvent(this.events, CitiesSeederEvents.SEEDING_FINISHED).subscribe(() =>
      this.cacheState.setLoaded(),
    );

    fromEvent<[string, WeightedIndex[]]>(this.events, IndexesEvents.NEW_INDEXES)
      .pipe(filter(([key]) => key === 'cities'))
      .subscribe(([_, indexes]) => this.cacheState.onIndexes(indexes));
  }

  private index() {
    const options = {
      includeScore: true,
      keys: this.cacheState.indexes,
    };
    const index = Fuse.createIndex(options.keys, this.cacheState.cities);
    this.fuse = new Fuse<City>(this.cacheState.cities, options, index);
    this.events.emit(CitiesRepositoryEvents.CITIES_READY);
  }

  query({ query }: CityQuery): Observable<CityQueryResult> {
    if (!this.cacheState.ready) {
      return from([]);
    }
    return from(this.fuse.search(query)).pipe(
      map(({ item, score }) => ({
        ...item,
        searchScore: 1 - (score || 0),
      })),
    );
  }

  getMaxPopulation(): Observable<number> {
    return of(this.cacheState.maxPopulation);
  }
}

class CacheState {
  private _maxPopulation = 0;
  private _cities: City[] = [];
  private _indexes: WeightedIndex[];
  private _citiesLoaded = false;
  private _doIndex$ = new Subject<void>();

  onNewCity(city: City) {
    this.cities.push(city);
    this._maxPopulation = Math.max(city.population, this._maxPopulation);
  }

  onIndexes(indexes: WeightedIndex[]) {
    this._indexes = indexes;
    if (this.ready) {
      this._doIndex$.next();
    }
  }

  setLoaded() {
    this._citiesLoaded = true;
    if (this.ready) {
      this._doIndex$.next();
    }
  }

  get maxPopulation() {
    return this._maxPopulation;
  }

  get cities() {
    return this._cities;
  }

  get indexes() {
    return this._indexes;
  }

  get ready() {
    return this._citiesLoaded && this._indexes;
  }

  get doIndex$(): Observable<void> {
    return this._doIndex$;
  }
}
