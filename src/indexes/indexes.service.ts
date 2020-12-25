import { Injectable, OnApplicationBootstrap } from '@nestjs/common';
import { EventEmitter2 } from '@nestjs/event-emitter';
import { IndexesEvents } from '../app-events';
import { WeightedIndex } from './interfaces/weighted.index';
import { IndexesRepository } from './indexes.repository';

@Injectable()
export class IndexesService implements OnApplicationBootstrap {
  constructor(private repo: IndexesRepository, private events: EventEmitter2) {}

  onApplicationBootstrap(): any {
    this.repo
      .getAll()
      .subscribe(([key, indexes]) => this.emitIndex(key, indexes));
  }

  private emitIndex(key: string, indexes: WeightedIndex[]) {
    this.events.emit(IndexesEvents.NEW_INDEXES, key, indexes);
  }
}
