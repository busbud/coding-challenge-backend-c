import { Test, TestingModule } from '@nestjs/testing';
import {
  CITIES_SEEDER_CONFIG_INJECTION_TOKEN,
  CitiesSeeder,
  TsvFileReader,
} from './cities.seeder';
import { count, first, mergeMap, take, takeLast } from 'rxjs/operators';
import { forkJoin, Observable } from 'rxjs';

const DATA_ROW_COUNT = 7237;
const DATA_PATH = 'data/cities_canada-usa.tsv';

describe('CitiesSeeder', () => {
  let service: CitiesSeeder;

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      providers: [
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
      country: 'CA',
      id: '5881791',
      location: { lat: 49.05798, lng: -122.25257 },
      population: 151683,
    };
    const randomMiddleCity = {
      alt_name: "Kollinsvil',Коллинсвиль",
      name: 'Collinsville',
      country: 'US',
      id: '4533909',
      location: { lat: 36.36454, lng: -95.83888 },
      population: 5606,
    };
    const lastCity = {
      alt_name: '',
      name: 'Cranberry Township',
      country: 'US',
      id: '8643098',
      location: { lat: 40.68496, lng: -80.10714 },
      population: 28098,
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
});
