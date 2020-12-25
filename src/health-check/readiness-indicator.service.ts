import { Injectable } from '@nestjs/common';
import {
  HealthCheckError,
  HealthIndicator,
  HealthIndicatorResult,
} from '@nestjs/terminus';
import { OnEvent } from '@nestjs/event-emitter';
import { CitiesRepositoryEvents } from '../app-events';

@Injectable()
export class ReadinessIndicator extends HealthIndicator {
  private ready = {
    cities: false,
  };

  @OnEvent(CitiesRepositoryEvents.CITIES_READY)
  setReady() {
    this.ready.cities = true;
  }

  async isHealthy(key): Promise<HealthIndicatorResult> {
    if (this.ready[key]) {
      return this.getStatus(key, true);
    }
    throw new HealthCheckError(`${key} not ready yet`, '');
  }
}
