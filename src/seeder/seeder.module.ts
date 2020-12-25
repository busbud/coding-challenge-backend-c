import { Module } from '@nestjs/common';
import { CitiesSeeder } from './cities.seeder';
import { CountriesRepository } from './countries.repository';
import { StatesRepository } from './states.repository';
import { CityMetadataMapper } from './city-metadata.mapper';
import { CITIES_SEEDER_CONFIG_INJECTION_TOKEN } from './cities.seeder';

@Module({
  providers: [
    CountriesRepository,
    StatesRepository,
    CityMetadataMapper,
    {
      provide: CITIES_SEEDER_CONFIG_INJECTION_TOKEN,
      useValue: {
        dataPath: 'data/cities_canada-usa.tsv',
      },
    },
    CitiesSeeder,
  ],
  exports: [CitiesSeeder],
})
export class SeederModule {}
