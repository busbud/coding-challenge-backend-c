import { Module } from '@nestjs/common';
import { CitiesModule } from './cities';
import { SuggestionsModule } from './sugesstions';
import { HealthCheckModule } from './health-check/health-check.module';
import { EventEmitterModule } from '@nestjs/event-emitter';

@Module({
  imports: [
    CitiesModule,
    SuggestionsModule,
    HealthCheckModule,
    EventEmitterModule.forRoot(),
  ],
})
export class AppModule {}
