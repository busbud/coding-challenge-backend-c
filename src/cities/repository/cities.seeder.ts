import { Inject, Injectable } from '@nestjs/common';
import { Observable } from 'rxjs';
import { City } from '../interfaces/city';
import { createReadStream, ReadStream } from 'fs';
import { map, mergeMap, share, take } from 'rxjs/operators';
import { createInterface, Interface } from 'readline';

export const CITIES_SEEDER_CONFIG_INJECTION_TOKEN =
  'CITIES_SEEDER_CONFIGURATION';

export interface CitiesSeederConfiguration {
  dataPath: string;
}

@Injectable()
export class CitiesSeeder {
  constructor(
    @Inject(CITIES_SEEDER_CONFIG_INJECTION_TOKEN)
    private readonly config: CitiesSeederConfiguration,
  ) {}

  loadCities(): Observable<City> {
    const rows$ = this.readRows();
    const columnNames$ = rows$.pipe(take(1));
    return columnNames$.pipe(
      mergeMap((columnNames) => {
        const cityMapper = new CityMapper(columnNames);
        return rows$.pipe(map((row) => cityMapper.toCity(row)));
      }),
    );
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

  toCity(row: string[]): City {
    const get = (col: string) => row[this.columnNames[col]];
    return {
      alt_name: get('alt_name'),
      name: get('name'),
      country: get('country'),
      id: get('id'),
      location: {
        lat: parseFloat(get('lat')),
        lng: parseFloat(get('long')),
      },
      population: parseInt(get('population')),
    };
  }
}

export class TsvFileReader {
  private interface: Interface;

  constructor(stream: ReadStream) {
    this.interface = createInterface({
      input: stream,
      crlfDelay: Infinity,
    });
  }

  readLines(): Observable<string> {
    return new Observable<string>((sub) => {
      this.interface.on('line', (line) => sub.next(line));
      this.interface.on('close', () => sub.complete());
    });
  }
}
