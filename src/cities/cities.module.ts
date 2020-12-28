import { Module } from '@nestjs/common';
import { CitiesService } from './cities.service';
import { CitiesController } from './cities.controller';
import { CitiesRepositoryModule } from './repository';
import { SeederModule } from '../seeder';

@Module({
  imports: [
    CitiesRepositoryModule.strategy(process.env.CITIES_REPOSITORY_STRATEGY),
    SeederModule,
  ],
  providers: [CitiesService],
  exports: [CitiesService],
  controllers: [CitiesController],
})
export class CitiesModule {}
