import { Injectable } from '@nestjs/common';
import { Observable, of, throwError } from 'rxjs';

@Injectable()
export class StatesRepository {
  private readonly stateCodesByCountry = {
    CA: {
      '01': 'AB',
      '02': 'BC',
      '03': 'MB',
      '04': 'NB',
      '05': 'NL',
      '07': 'NS',
      '08': 'ON',
      '09': 'PE',
      '10': 'QC',
      '11': 'SK',
      '12': 'YT',
      '13': 'NT',
      '14': 'NU',
    },
  };

  getStateCode(countryCode: string, stateCode: string): Observable<string> {
    const stateCodes = this.stateCodesByCountry[countryCode];
    if (!stateCodes) {
      return of(stateCode);
    }
    const code = stateCodes[stateCode];
    if (!code) {
      return throwError(
        `Code ${stateCode} not found for country ${countryCode}`,
      );
    }
    return of(code);
  }
}
