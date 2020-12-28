import { DynamicModule, Module } from '@nestjs/common';
import { CITIES_REPOSITORY_INJECTION_TOKEN } from './cities.repository';
import { CitiesInMemoryRepository } from './cities.inmemory.repository';
import {
  CITIES_IN_MEMORY_CONFIG,
  CITIES_MONGO_CONFIGURATION,
  memoryConfig,
  mongooseRootConfig,
  mongooseSchemaConfig,
  mongoRepositoryConfig,
} from './config';
import { ConfigService } from '@nestjs/config';
import { MongoInitializerService } from './mongo-initializer.service';
import { MongooseModule } from '@nestjs/mongoose';
import { CitiesMongoRepository } from './cities.mongo.repository';

@Module({})
export class CitiesRepositoryModule {
  static strategy(strategy: string | undefined) {
    switch (strategy) {
      case 'memory':
        return this.memory();
      case 'mongo':
        return this.mongo();
      default:
        throw new Error(
          `Invalid strategy ${strategy}, choose on of [memory, mongo]`,
        );
    }
  }

  static memory(): DynamicModule {
    return {
      module: CitiesRepositoryModule,
      providers: [
        {
          provide: CITIES_IN_MEMORY_CONFIG,
          useFactory: memoryConfig,
          inject: [ConfigService],
        },
        {
          provide: CITIES_REPOSITORY_INJECTION_TOKEN,
          useClass: CitiesInMemoryRepository,
        },
      ],
      exports: [CITIES_REPOSITORY_INJECTION_TOKEN],
    };
  }

  static mongo(): DynamicModule {
    return {
      module: CitiesRepositoryModule,
      imports: [
        MongooseModule.forFeatureAsync([
          {
            name: 'City',
            useFactory: mongooseSchemaConfig,
            inject: [ConfigService],
          },
        ]),
        MongooseModule.forRootAsync({
          useFactory: mongooseRootConfig,
          inject: [ConfigService],
        }),
      ],
      providers: [
        {
          provide: CITIES_MONGO_CONFIGURATION,
          useFactory: mongoRepositoryConfig,
          inject: [ConfigService],
        },
        {
          provide: CITIES_REPOSITORY_INJECTION_TOKEN,
          useClass: CitiesMongoRepository,
        },
        MongoInitializerService,
      ],
      exports: [CITIES_REPOSITORY_INJECTION_TOKEN],
    };
  }
}
