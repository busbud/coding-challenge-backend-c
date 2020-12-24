import { Injectable } from '@nestjs/common';
import {
  HealthCheckError,
  HealthIndicator,
  HealthIndicatorResult,
} from '@nestjs/terminus';
import { OnEvent } from '@nestjs/event-emitter';

@Injectable()
export class CitiesIndicator extends HealthIndicator {
  private ready = false;

  @OnEvent('cities.ready')
  setReady(ready: boolean) {
    this.ready = ready;
  }

  async isHealthy(key): Promise<HealthIndicatorResult> {
    if (this.ready) {
      return this.getStatus(key, true);
    }
    throw new HealthCheckError('Cities not ready yet', '');
  }
}
