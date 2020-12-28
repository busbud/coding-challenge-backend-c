import { Injectable, Logger } from '@nestjs/common';
import { fromEvent } from 'rxjs';
import { City } from '../interfaces/city';
import { EventEmitter2 } from '@nestjs/event-emitter';
import { CitiesRepositoryEvents, CitiesSeederEvents } from '../../app-events';
import { MongoInitializerState } from './mongo-initializer.state';
import { InjectModel } from '@nestjs/mongoose';
import { Model } from 'mongoose';
import { CityDocument } from './city.schema';

@Injectable()
export class MongoInitializerService {
  private readonly logger = new Logger(MongoInitializerService.name);
  readonly state = new MongoInitializerState();

  constructor(
    private events: EventEmitter2,
    @InjectModel('City') private readonly cityModel: Model<CityDocument>,
  ) {
    this.listenEvents();
  }

  private listenEvents() {
    this.state.ready$.subscribe(() => {
      this.events.emit(CitiesRepositoryEvents.CITIES_READY);
      this.logger.log('Cities ready to be queried');
    });
    fromEvent<City>(
      this.events,
      CitiesSeederEvents.NEW_CITY,
    ).subscribe((city) => this.state.enqueueInsert(city));

    fromEvent(this.events, CitiesSeederEvents.SEEDING_FINISHED).subscribe(
      () => {
        this.insertCities(this.state.insertQueue);
      },
    );
  }

  private insertCities(cities: City[]) {
    this.cityModel
      .insertMany(cities)
      .catch((err) => {
        this.logger.error(`Error in bulk insert: ${err.message}`);
      })
      .finally(() => this.state.finishSeeding());
  }

  async init() {
    const seedingNeeded = await this.needsSeeding();
    if (seedingNeeded) {
      this.events.emit(CitiesRepositoryEvents.SEEDING_REQUESTED);
    } else {
      this.state.finishSeeding();
      await this.fetchMaxPopulation();
    }
  }

  private async needsSeeding(): Promise<boolean> {
    const citiesCount = await this.cityModel.countDocuments();
    this.logger.log(`Cities count in DB is ${citiesCount}`);
    if (citiesCount > 0) {
      this.logger.log(`No seeding needed`);
      return false;
    }
    this.logger.log(`Asking for cities to be seeded`);
    return true;
  }

  private async fetchMaxPopulation(): Promise<void> {
    const [biggestCity] = await this.cityModel
      .find()
      .sort({ population: -1 })
      .limit(1);
    this.logger.log(
      `Fetched max population from DB: ${biggestCity.population}`,
    );
    this.state.maxPopulation = biggestCity.population;
  }
}
