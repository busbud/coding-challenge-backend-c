import { Test, TestingModule } from '@nestjs/testing';
import {
  CITIES_SEEDER_CONFIG_INJECTION_TOKEN,
  CitiesSeeder,
} from './cities.seeder';
import { count, first, mergeMap, take, takeLast } from 'rxjs/operators';
import { forkJoin, Observable } from 'rxjs';
import { TsvFileReader } from './tsv.file-reader';
import { CityMetadataMapper } from './city-metadata.mapper';
import { CountriesRepository } from './countries.repository';
import { StatesRepository } from './states.repository';
import { EventEmitter2, EventEmitterModule } from '@nestjs/event-emitter';
import { CitiesSeederEvents } from '../app-events';
import anything = jasmine.anything;
import { LocationModule } from '../location';

const DATA_ROW_COUNT = 7237;
const DATA_PATH = 'data/cities_canada-usa.tsv';

describe('CitiesSeeder', () => {
  let service: CitiesSeeder;
  let events: EventEmitter2;

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      imports: [EventEmitterModule.forRoot(), LocationModule],
      providers: [
        CountriesRepository,
        StatesRepository,
        CityMetadataMapper,
        {
          provide: CITIES_SEEDER_CONFIG_INJECTION_TOKEN,
          useValue: {
            dataPath: DATA_PATH,
          },
        },
        CitiesSeeder,
      ],
    }).compile();

    service = module.get<CitiesSeeder>(CitiesSeeder);
    events = module.get<EventEmitter2>(EventEmitter2);
  });

  it('should be defined', () => {
    expect(service).toBeDefined();
  });

  it('should read all rows', (done) => {
    const count$ = service.loadCities().pipe(count());
    count$.subscribe((count) => {
      expect(count).toBe(DATA_ROW_COUNT);
      done();
    });
  });

  it('should parse data correctly', (done) => {
    const firstCity = {
      alt_name: 'Abbotsford,YXX,Абботсфорд',
      name: 'Abbotsford',
      country: 'Canada',
      state: 'BC',
      geohash: 'c29',
      normalized_name: 'Abbotsford',
      id: '5881791',
      location: { lat: 49.05798, lng: -122.25257 },
      population: 151683,
    };
    const randomMiddleCity = {
      alt_name: "Kollinsvil',Коллинсвиль",
      name: 'Collinsville',
      normalized_name: 'Collinsville',
      country: 'USA',
      geohash: '9y7',
      id: '4533909',
      location: { lat: 36.36454, lng: -95.83888 },
      population: 5606,
      state: 'OK',
    };
    const lastCity = {
      alt_name: '',
      name: 'Cranberry Township',
      normalized_name: 'Cranberry Township',
      country: 'USA',
      geohash: 'dpp',
      id: '8643098',
      location: { lat: 40.68496, lng: -80.10714 },
      population: 28098,
      state: 'PA',
    };
    const cities$ = service.loadCities();
    const last$ = cities$.pipe(takeLast(1));
    const random$ = cities$.pipe(first((_, i) => i == 2336));
    const first$ = cities$.pipe(take(1));
    forkJoin([first$, random$, last$]).subscribe(([first, random, last]) => {
      expect(first).toEqual(firstCity);
      expect(random).toEqual(randomMiddleCity);
      expect(last).toEqual(lastCity);
      done();
    });
  });

  it('should share observable', (done) => {
    let callCount = 0;
    spyOn(TsvFileReader.prototype, 'readLines').and.callFake(
      () =>
        new Observable((sub) => {
          callCount++;
          sub.next('1');
          sub.next('2');
          sub.complete();
        }),
    );

    const rows$ = service.readRows();
    const first$ = rows$.pipe(take(1));

    first$.pipe(mergeMap(() => rows$)).subscribe(() => {
      expect(callCount).toBe(1);
      done();
    });
  });

  it('should initialize and emit events', (done) => {
    const spy = spyOn(events, 'emit').and.callThrough();
    service.onApplicationBootstrap();
    events.addListener(CitiesSeederEvents.SEEDING_FINISHED, () => {
      for (let i = 0; i < DATA_ROW_COUNT; i++) {
        expect(spy).toBeCalledWith(CitiesSeederEvents.NEW_CITY, anything());
      }
      done();
    });
  });
});
