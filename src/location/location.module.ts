import { Module } from '@nestjs/common';
import {
  LOCATION_CONFIG_INJECTION_TOKEN,
  LocationService,
} from './location.service';

@Module({
  providers: [
    LocationService,
    {
      provide: LOCATION_CONFIG_INJECTION_TOKEN,
      useValue: {
        geohashPrecision: 3,
      },
    },
  ],
  exports: [LocationService],
})
export class LocationModule {}
