import { Injectable } from '@nestjs/common';
import { Location } from './interfaces/location';

@Injectable()
export class LocationParser {
  parse(lat?: number, lng?: number): Location | undefined {
    if (!lat || !lng) {
      return undefined;
    }
    return {
      lat: lat,
      lng: lng,
    };
  }
}
