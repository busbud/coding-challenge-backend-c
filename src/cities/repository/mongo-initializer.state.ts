import { Subject } from 'rxjs';
import { City } from '../interfaces/city';

export class MongoInitializerState {
  private _bulkInsert: City[] = [];
  private readonly _ready$ = new Subject<void>();
  private _maxPopulation = 0;
  private _pendingSeed = true;

  enqueueInsert(city: City) {
    this._bulkInsert.push(city);
    this._maxPopulation = Math.max(city.population, this._maxPopulation);
  }

  get insertQueue() {
    return this._bulkInsert;
  }

  finishSeeding() {
    this._pendingSeed = false;
    this._bulkInsert = [];
    if (this.ready) {
      this._ready$.next();
    }
  }

  set maxPopulation(pop: number) {
    this._maxPopulation = pop;
    if (this.ready) {
      this._ready$.next();
    }
  }

  get maxPopulation() {
    return this._maxPopulation;
  }

  get ready() {
    return !!this._maxPopulation && !this._pendingSeed;
  }

  get notReadyMessage(): string {
    const missing: string[] = [];
    if (!this._maxPopulation) {
      missing.push('missing maxPopulation');
    }
    if (this._pendingSeed) {
      missing.push('seeding not finished');
    }
    return missing.join(', ');
  }

  get ready$() {
    return this._ready$;
  }
}
