import { Controller, Get } from '@nestjs/common';
import { HealthCheck, HealthCheckService } from '@nestjs/terminus';
import { ReadinessIndicator } from './readiness-indicator.service';

@Controller('health')
export class HealthCheckController {
  constructor(
    private health: HealthCheckService,
    private cities: ReadinessIndicator,
  ) {}

  @Get()
  @HealthCheck()
  check() {
    return this.health.check([() => this.cities.isHealthy('cities')]);
  }
}
