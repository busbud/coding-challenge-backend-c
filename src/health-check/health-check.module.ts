import { Module } from '@nestjs/common';
import { HealthCheckController } from './health-check.controller';
import { TerminusModule } from '@nestjs/terminus';
import { CitiesIndicator } from './cities.indicator';

@Module({
  imports: [TerminusModule],
  providers: [CitiesIndicator],
  controllers: [HealthCheckController],
})
export class HealthCheckModule {}
