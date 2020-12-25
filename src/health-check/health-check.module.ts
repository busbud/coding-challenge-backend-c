import { Module } from '@nestjs/common';
import { HealthCheckController } from './health-check.controller';
import { TerminusModule } from '@nestjs/terminus';
import { ReadinessIndicator } from './readiness-indicator.service';

@Module({
  imports: [TerminusModule],
  providers: [ReadinessIndicator],
  controllers: [HealthCheckController],
})
export class HealthCheckModule {}
