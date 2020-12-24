import { Controller, Get } from '@nestjs/common';
import { HealthCheck, HealthCheckService } from '@nestjs/terminus';
import { CitiesIndicator } from './cities.indicator';

@Controller('health')
export class HealthCheckController {
  constructor(
    private health: HealthCheckService,
    private cities: CitiesIndicator,
  ) {}

  @Get()
  @HealthCheck()
  check() {
    return this.health.check([() => this.cities.isHealthy('cities')]);
  }
}
