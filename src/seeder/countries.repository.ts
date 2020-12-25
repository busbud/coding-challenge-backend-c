import { Injectable } from '@nestjs/common';
import { Observable, of, throwError } from 'rxjs';

@Injectable()
export class CountriesRepository {
  private readonly countriesByCode = {
    CA: 'Canada',
    US: 'USA',
  };

  getCountryName(countryCode: string): Observable<string> {
    const name = this.countriesByCode[countryCode];
    return name
      ? of(name)
      : throwError(`Country name not found for ${countryCode}`);
  }
}
