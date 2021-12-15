import {Module} from '@nestjs/common';
import {SuggestionService} from "./service/suggestion.service";
import {SuggestionController} from "./controller/suggestion.controller";
import {CityEntity} from "./entity/city.entity";
import {TypeOrmModule} from '@nestjs/typeorm';

@Module({
    imports: [TypeOrmModule.forFeature([CityEntity])],
    controllers: [SuggestionController],
    providers: [SuggestionService]
})
export class SuggestionModule {
}
