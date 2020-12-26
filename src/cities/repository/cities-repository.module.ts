import { Module } from '@nestjs/common';
import { CITIES_REPOSITORY_INJECTION_TOKEN } from './cities.repository';
import {
  CITIES_IN_MEMORY_CONFIG_INJECTION_TOKEN,
  CitiesInMemoryRepository,
} from './inmemory-cities.repository';
import config from './config';
import { ConfigService } from '@nestjs/config';

@Module({
  providers: [
    {
      provide: CITIES_REPOSITORY_INJECTION_TOKEN,
      useClass: CitiesInMemoryRepository,
    },
    {
      provide: CITIES_IN_MEMORY_CONFIG_INJECTION_TOKEN,
      useFactory: config,
      inject: [ConfigService],
    },
  ],
  exports: [CITIES_REPOSITORY_INJECTION_TOKEN],
})
export class CitiesRepositoryModule {}
