import { Module } from '@nestjs/common';
import { CITIES_REPOSITORY_INJECTION_TOKEN } from './cities.repository';
import { CitiesInMemoryRepository } from './inmemory-cities.repository';

@Module({
  providers: [
    {
      provide: CITIES_REPOSITORY_INJECTION_TOKEN,
      useClass: CitiesInMemoryRepository,
    },
  ],
  exports: [CITIES_REPOSITORY_INJECTION_TOKEN],
})
export class CitiesRepositoryModule {}
