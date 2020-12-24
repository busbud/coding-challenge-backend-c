import { Module } from '@nestjs/common';
import { CitiesService } from './cities.service';
import { CitiesController } from './cities.controller';
import { CitiesRepositoryModule } from './repository';

@Module({
  imports: [CitiesRepositoryModule],
  providers: [CitiesService],
  exports: [CitiesService],
  controllers: [CitiesController],
})
export class CitiesModule {}
