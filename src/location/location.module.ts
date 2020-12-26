import { Module } from '@nestjs/common';
import {
  LOCATION_CONFIG_INJECTION_TOKEN,
  LocationService,
} from './location.service';
import config from './config';
import { ConfigService } from '@nestjs/config';

@Module({
  providers: [
    LocationService,
    {
      provide: LOCATION_CONFIG_INJECTION_TOKEN,
      useFactory: config,
      inject: [ConfigService],
    },
  ],
  exports: [LocationService],
})
export class LocationModule {}
