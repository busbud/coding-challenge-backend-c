import {
  Inject,
  Injectable,
  Logger,
  OnApplicationBootstrap,
} from '@nestjs/common';
import { Observable } from 'rxjs';
import { City } from '../cities';
import { createReadStream } from 'fs';
import { map, mergeMap, share, take } from 'rxjs/operators';
import { CityMetadataMapper } from './city-metadata.mapper';
import { TsvFileReader } from './tsv.file-reader';
import { EventEmitter2 } from '@nestjs/event-emitter';
import { CitiesSeederEvents } from '../app-events';

export const CITIES_SEEDER_CONFIG_INJECTION_TOKEN =
  'CITIES_SEEDER_CONFIGURATION';

export interface CitiesSeederConfiguration {
  dataPath: string;
}

@Injectable()
export class CitiesSeeder implements OnApplicationBootstrap {
  private readonly logger = new Logger(CitiesSeeder.name);
  constructor(
    @Inject(CITIES_SEEDER_CONFIG_INJECTION_TOKEN)
    private readonly config: CitiesSeederConfiguration,
    private readonly cityMetadataMapper: CityMetadataMapper,
    private events: EventEmitter2,
  ) {}

  loadCities(): Observable<City> {
    const rows$ = this.readRows();
    const columnNames$ = rows$.pipe(take(1));
    return columnNames$.pipe(
      mergeMap((columnNames) => {
        const cityMapper = new CityMapper(columnNames);
        return rows$.pipe(
          map((row) => cityMapper.toCity(row)),
          mergeMap((city) => this.cityMetadataMapper.map(city)),
        );
      }),
    );
  }

  onApplicationBootstrap(): any {
    this.loadCities().subscribe({
      next: (city) => this.events.emit(CitiesSeederEvents.NEW_CITY, city),
      complete: () => {
        this.events.emit(CitiesSeederEvents.SEEDING_FINISHED);
        this.logger.log('Successfully loaded cities');
      },
      error: (err) => {
        this.logger.error(`Error loading cities ${err}`);
      },
    });
  }

  readRows(): Observable<string[]> {
    return new TsvFileReader(createReadStream(this.config.dataPath))
      .readLines()
      .pipe(
        share(),
        map((line) => line.split('\t')),
      );
  }
}

class CityMapper {
  private readonly columnNames: Record<string, number>;

  constructor(columnNames: string[]) {
    this.columnNames = columnNames.reduce(
      (acc, col, index) => ({
        ...acc,
        [col]: index,
      }),
      {},
    );
  }

  toCity(row: string[]): Omit<City, 'normalized_name' | 'geohash'> {
    const get = (col: string) => row[this.columnNames[col]];
    return {
      alt_name: get('alt_name'),
      name: get('name'),
      country: get('country'),
      state: get('admin1'),
      id: get('id'),
      location: {
        lat: parseFloat(get('lat')),
        lng: parseFloat(get('long')),
      },
      population: parseInt(get('population')),
    };
  }
}
