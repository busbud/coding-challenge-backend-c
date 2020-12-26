import { Injectable } from '@nestjs/common';
import { City } from '../cities';
import { CountriesRepository } from './countries.repository';
import { StatesRepository } from './states.repository';
import { EMPTY, forkJoin, Observable } from 'rxjs';
import { catchError, map } from 'rxjs/operators';
import { LocationService } from '../location/location.service';

@Injectable()
export class CityMetadataMapper {
  constructor(
    private readonly countriesRepository: CountriesRepository,
    private readonly statesRepository: StatesRepository,
    private readonly locationParser: LocationService,
  ) {}

  map({
    name,
    state,
    country,
    location,
    ...rest
  }: Omit<City, 'normalized_name' | 'geohash'>): Observable<City> {
    const countryName$ = this.countriesRepository.getCountryName(country);
    const stateName$ = this.statesRepository.getStateCode(country, state);
    return forkJoin([countryName$, stateName$]).pipe(
      map(([country, state]) => ({
        ...rest,
        name,
        state,
        country,
        location,
        geohash: this.locationParser.geohashEncode(location),
        normalized_name: name.normalize('NFD').replace(/[\u0300-\u036f]/g, ''),
      })),
      catchError((err) => {
        console.error('Error mapping city', err, 'Skipping...');
        return EMPTY;
      }),
    );
  }
}
