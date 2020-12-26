import { Module } from '@nestjs/common';
import { CitiesModule } from './cities';
import { SuggestionsModule } from './sugesstions';
import { HealthCheckModule } from './health-check/health-check.module';
import { EventEmitterModule } from '@nestjs/event-emitter';
import { IndexesModule } from './indexes';
import { MetricsModule } from './metrics/metrics.module';
import { DebugModule } from './debug/debug.module';

@Module({
  imports: [
    IndexesModule,
    CitiesModule,
    SuggestionsModule,
    HealthCheckModule,
    EventEmitterModule.forRoot(),
    MetricsModule,
    DebugModule,
  ],
})
export class AppModule {}
