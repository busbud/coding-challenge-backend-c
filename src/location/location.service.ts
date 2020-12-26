import { Inject, Injectable } from '@nestjs/common';
import { Location } from './interfaces/location';
import { encode } from 'ngeohash';

export const LOCATION_CONFIG_INJECTION_TOKEN = 'LOCATION_CONFIGURATION';

export type LocationConfig = {
  geohashPrecision: number;
};

// Based on the max distance within a geohash box
const DIVIDERS_PER_PRECISION = [4500, 1000, 150, 50, 10, 5, 1, 1, 1, 1, 1, 1];
@Injectable()
export class LocationService {
  constructor(
    @Inject(LOCATION_CONFIG_INJECTION_TOKEN) private config: LocationConfig,
  ) {}

  parse(lat?: number, lng?: number): Location | undefined {
    if (!lat || !lng) {
      return undefined;
    }
    return {
      lat: lat,
      lng: lng,
    };
  }

  geohashEncode(location: Location): string {
    return encode(location.lat, location.lng, this.config.geohashPrecision);
  }

  get nearByDistanceDivider(): number {
    return DIVIDERS_PER_PRECISION[this.config.geohashPrecision];
  }

  static distanceBetween(a: Location, b: Location) {
    const R = 3958.8; // Radius of the Earth in miles
    const rlat1 = a.lat * (Math.PI / 180); // Convert degrees to radians
    const rlat2 = b.lat * (Math.PI / 180); // Convert degrees to radians
    const difflat = rlat2 - rlat1; // Radian difference (latitudes)
    const difflon = (b.lng - a.lng) * (Math.PI / 180); // Radian difference (longitudes)

    return (
      2 *
      R *
      Math.asin(
        Math.sqrt(
          Math.sin(difflat / 2) * Math.sin(difflat / 2) +
            Math.cos(rlat1) *
              Math.cos(rlat2) *
              Math.sin(difflon / 2) *
              Math.sin(difflon / 2),
        ),
      )
    );
  }
}
