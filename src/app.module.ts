import {Module} from '@nestjs/common';
import {TypeOrmModule} from '@nestjs/typeorm';
import {TerminusModule} from '@nestjs/terminus';
import HealthModule from './health/health.module';
import {SuggestionModule} from './suggestion/suggestion.module';
import {CityModule} from './city/city.module';

@Module({
    imports: [
        TypeOrmModule.forRoot(),
        SuggestionModule,
        TerminusModule,
        HealthModule,
        CityModule
    ]
})
export class AppModule {
}
