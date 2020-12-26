import { Module } from '@nestjs/common';
import { CitiesModule } from './cities';
import { SuggestionsModule } from './sugesstions';
import { HealthCheckModule } from './health-check/health-check.module';
import { EventEmitterModule } from '@nestjs/event-emitter';
import { MetricsModule } from './metrics/metrics.module';
import { DebugModule } from './debug/debug.module';
import { ConfigModule } from '@nestjs/config';

@Module({
  imports: [
    ConfigModule.forRoot({
      isGlobal: true,
    }),
    CitiesModule,
    SuggestionsModule,
    HealthCheckModule,
    EventEmitterModule.forRoot(),
    MetricsModule,
    DebugModule,
  ],
})
export class AppModule {}
