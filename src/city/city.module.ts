import {CacheModule, Module, OnModuleInit} from '@nestjs/common';
import {TypeOrmModule} from '@nestjs/typeorm';
import * as redisStore from 'cache-manager-redis-store';
import {CityEntity} from './city.entity';
import {CityService} from '../city/city.service';

@Module({
    imports: [TypeOrmModule.forFeature([CityEntity]),
        CacheModule.register({
            store: redisStore,
            host: process.env.REDISHOST,
            port: process.env.REDISPORT,
            auth_pass: process.env.REDISPASSWORD,
        }),
    ],
    providers: [CityService],

})
export class CityModule implements OnModuleInit {

    constructor(
        private cityService: CityService,
    ) {
    }

    /**
     * We only seed the database because it's a training project, in production we won't do this from a shell script
     */
    onModuleInit(): any {
        this.cityService.seedDatabase();
    }
}
