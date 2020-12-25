import { Injectable } from '@nestjs/common';
import { EventEmitter2 } from '@nestjs/event-emitter';
import { WeightedIndex } from './interfaces/weighted.index';
import { from, Observable, of } from 'rxjs';

@Injectable()
export class IndexesRepository {
  private indexes: Record<string, WeightedIndex[]> = {
    cities: [
      {
        name: 'name',
        weight: 2,
      },
      {
        name: 'normalized_name',
        weight: 1,
      },
    ],
  };

  constructor(private events: EventEmitter2) {}

  getWeightedIndexes(key: string): Observable<WeightedIndex[]> {
    return of(this.indexes[key]);
  }

  getAll(): Observable<[string, WeightedIndex[]]> {
    return from(Object.entries(this.indexes));
  }
}
