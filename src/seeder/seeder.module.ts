import { Module } from '@nestjs/common';
import { CitiesSeeder } from './cities.seeder';
import { CountriesRepository } from './countries.repository';
import { StatesRepository } from './states.repository';
import { CityMetadataMapper } from './city-metadata.mapper';
import { CITIES_SEEDER_CONFIG_INJECTION_TOKEN } from './cities.seeder';
import { LocationModule } from '../location';
import config from './config';

@Module({
  imports: [LocationModule],
  providers: [
    CountriesRepository,
    StatesRepository,
    CityMetadataMapper,
    {
      provide: CITIES_SEEDER_CONFIG_INJECTION_TOKEN,
      useFactory: config,
    },
    CitiesSeeder,
  ],
  exports: [CitiesSeeder],
})
export class SeederModule {}
