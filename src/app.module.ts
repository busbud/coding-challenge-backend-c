import {Module} from '@nestjs/common';
import {SuggestionModule} from './suggestion/suggestion.module';
import HealthModule from './health/health.module';
import {TypeOrmModule} from '@nestjs/typeorm';
import {TerminusModule} from '@nestjs/terminus';


@Module({
    imports: [
        TypeOrmModule.forRoot(),
        SuggestionModule,
        TerminusModule,
        HealthModule
    ]
})
export class AppModule {
}
