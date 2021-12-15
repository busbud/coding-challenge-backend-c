import {CacheModule, Module} from '@nestjs/common';
import {SuggestionService} from "./service/suggestion.service";
import {SuggestionController} from "./controller/suggestion.controller";
import {CityEntity} from "./entity/city.entity";
import {TypeOrmModule} from '@nestjs/typeorm';
import * as redisStore from 'cache-manager-redis-store';

@Module({
    imports: [TypeOrmModule.forFeature([CityEntity]),
        CacheModule.register({
            store: redisStore,
            host: 'localhost',
            port: 6379
        })],
    controllers: [SuggestionController],
    providers: [SuggestionService]
})
export class SuggestionModule {
}
