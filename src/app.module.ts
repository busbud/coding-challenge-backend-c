import {Module} from '@nestjs/common';
import {APP_GUARD} from '@nestjs/core';
import {TypeOrmModule} from '@nestjs/typeorm';
import {ConfigModule} from '@nestjs/config';
import {TerminusModule} from '@nestjs/terminus';
import {ThrottlerGuard, ThrottlerModule} from '@nestjs/throttler';
import HealthModule from './health/health.module';
import {SuggestionModule} from './suggestion/suggestion.module';
import {CityModule} from './city/city.module';
import {AuthModule} from './auth/auth.module';

@Module({
    imports: [
        ConfigModule.forRoot(),
        TypeOrmModule.forRoot(),
        SuggestionModule,
        TerminusModule,
        HealthModule,
        CityModule,
        ThrottlerModule.forRoot({
            ttl: 60,
            limit: 20,
        }),
        AuthModule,
    ],
    providers: [
        {
            provide: APP_GUARD,
            useClass: ThrottlerGuard,
        }],
    exports: [TypeOrmModule],
})
export class AppModule {
}
