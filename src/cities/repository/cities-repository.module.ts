import { Module } from '@nestjs/common';
import { CITIES_REPOSITORY_INJECTION_TOKEN } from './cities.repository';
import { CitiesInMemoryRepository } from './inmemory-cities.repository';
import {
  CITIES_SEEDER_CONFIG_INJECTION_TOKEN,
  CitiesSeeder,
} from './cities.seeder';

@Module({
  providers: [
    CitiesSeeder,
    {
      provide: CITIES_SEEDER_CONFIG_INJECTION_TOKEN,
      useValue: {
        dataPath: 'data/cities_canada-usa.tsv',
      },
    },
    {
      provide: CITIES_REPOSITORY_INJECTION_TOKEN,
      useClass: CitiesInMemoryRepository,
    },
  ],
  exports: [CITIES_REPOSITORY_INJECTION_TOKEN],
})
export class CitiesRepositoryModule {}
