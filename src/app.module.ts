import {Module} from '@nestjs/common';
import {APP_GUARD} from '@nestjs/core';
import {TypeOrmModule} from '@nestjs/typeorm';
import {TerminusModule} from '@nestjs/terminus';
import {ThrottlerGuard, ThrottlerModule} from '@nestjs/throttler';
import HealthModule from './health/health.module';
import {SuggestionModule} from './suggestion/suggestion.module';
import {CityModule} from './city/city.module';

@Module({
    imports: [
        TypeOrmModule.forRoot(),
        SuggestionModule,
        TerminusModule,
        HealthModule,
        CityModule,
        ThrottlerModule.forRoot({
            ttl: 60,
            limit: 20,
        }),
    ],
    providers: [
        {
            provide: APP_GUARD,
            useClass: ThrottlerGuard,
        }],
})
export class AppModule {
}
