import {Module} from '@nestjs/common';
import {SuggestionModule} from './suggestion/suggestion.module';
import {TypeOrmModule} from '@nestjs/typeorm';

@Module({
    imports: [
        TypeOrmModule.forRoot(),
        SuggestionModule
    ]
})
export class AppModule {
}
