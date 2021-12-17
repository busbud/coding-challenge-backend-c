import {CacheModule, Module} from '@nestjs/common';
import {TypeOrmModule} from '@nestjs/typeorm';
import * as redisStore from 'cache-manager-redis-store';
import {SuggestionService} from './service/suggestion.service';
import {CityService} from './../city/city.service';
import {SuggestionController} from './controller/suggestion.controller';
import {CityEntity} from './../city/city.entity';

@Module({
    imports: [TypeOrmModule.forFeature([CityEntity]),
        CacheModule.register({
            store: redisStore,
            host: process.env.REDISHOST,
            port: process.env.REDISPORT,
            auth_pass: process.env.REDISPASSWORD,
        })],
    controllers: [SuggestionController],
    providers: [SuggestionService, CityService],
})
export class SuggestionModule {

}
