import { Injectable } from '@nestjs/common';
import { City } from '../cities';
import { CountriesRepository } from './countries.repository';
import { StatesRepository } from './states.repository';
import { EMPTY, forkJoin, Observable } from 'rxjs';
import { catchError, map } from 'rxjs/operators';

@Injectable()
export class CityMetadataMapper {
  constructor(
    private readonly countriesRepository: CountriesRepository,
    private readonly statesRepository: StatesRepository,
  ) {}

  map({
    name,
    state,
    country,
    ...rest
  }: Omit<City, 'normalized_name'>): Observable<City> {
    const countryName$ = this.countriesRepository.getCountryName(country);
    const stateName$ = this.statesRepository.getStateCode(country, state);
    return forkJoin([countryName$, stateName$]).pipe(
      map(([country, state]) => ({
        ...rest,
        name,
        state,
        country,
        normalized_name: name.normalize('NFD').replace(/[\u0300-\u036f]/g, ''),
      })),
      catchError((err) => {
        console.error('Error mapping city', err, 'Skipping...');
        return EMPTY;
      }),
    );
  }
}
