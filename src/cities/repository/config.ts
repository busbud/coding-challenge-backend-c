import { ConfigService } from '@nestjs/config';
import { WeightedIndex } from '../interfaces/weighted-index';
import { MongooseModuleOptions } from '@nestjs/mongoose';
import { ModelDefinition } from '@nestjs/mongoose/dist/interfaces/model-definition.interface';
import { CitySchema } from './city.schema';
import fuzzySearching from 'mongoose-fuzzy-searching';

export const CITIES_IN_MEMORY_CONFIG = 'CITIES_IN_MEMORY_CONFIGURATION';

export const CITIES_MONGO_CONFIGURATION = 'CITIES_MONGO_CONFIGURATION';

export interface CitiesMongoRepositoryConfiguration {
  scoreThreshold: number;
}

export interface CitiesInMemoryConfiguration {
  indexes: WeightedIndex[];
  scoreThreshold: number;
  textDistance: number;
}

export function getIndexes(c: ConfigService, defaultIndexes) {
  const env = c.get<string>('CITIES_REPOSITORY_INDEXES');
  if (!env) {
    return defaultIndexes;
  }
  // name1:weight1,name2:weight2 with float support
  if (!/^[\w\d.]+:(\d+\.?\d*)+(,[\w\d.]+:(\d+\.?\d*))*$/.test(env)) {
    throw new Error(`Invalid CITIES_REPOSITORY_INDEXES syntax: ${env}`);
  }
  return env.split(',').map((index) => {
    const [name, weight] = index.split(':');
    return { name, weight: parseInt(weight) };
  });
}

export function memoryConfig(c: ConfigService): CitiesInMemoryConfiguration {
  return {
    indexes: getIndexes(c, [
      {
        name: 'name',
        weight: 2,
      },
      {
        name: 'normalized_name',
        weight: 1,
      },
    ]),
    textDistance: c.get<number>('CITIES_REPOSITORY_FUSE_TEXT_DISTANCE', 5),
    scoreThreshold: c.get<number>('CITIES_REPOSITORY_SCORE_THRESHOLD', 0.4),
  };
}

export function mongooseRootConfig(c: ConfigService): MongooseModuleOptions {
  const uri = c.get('CITIES_REPOSITORY_MONGO_URI');
  if (!uri) {
    throw new Error('CITIES_REPOSITORY_MONGO_URI env missing');
  }
  return {
    uri,
  };
}

export function mongooseSchemaConfig(
  c: ConfigService,
): ModelDefinition['schema'] {
  const indexes = getIndexes(c, [
    {
      name: 'name',
      weight: 1,
    },
  ]);
  const schema = CitySchema;
  schema.plugin(fuzzySearching, {
    fields: indexes.map((i) => ({ ...i, prefixOnly: true })),
  });
  return schema;
}

export function mongoRepositoryConfig(
  c: ConfigService,
): CitiesMongoRepositoryConfiguration {
  return {
    scoreThreshold: c.get<number>('CITIES_REPOSITORY_SCORE_THRESHOLD', 0.4),
  };
}
