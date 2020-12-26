import { CitiesInMemoryConfiguration } from './inmemory-cities.repository';
import { ConfigService } from '@nestjs/config';

const DEFAULT_INDEXES = [
  {
    name: 'name',
    weight: 2,
  },
  {
    name: 'normalized_name',
    weight: 1,
  },
];

export default function (c: ConfigService): CitiesInMemoryConfiguration {
  function getIndexes() {
    const env = c.get<string>('CITIES_REPOSITORY_FUSE_INDEXES');
    if (!env) {
      return DEFAULT_INDEXES;
    }
    // name1:weight1,name2:weight2 with float support
    if (!/^[\w\d.]+:(\d+\.?\d*)+(,[\w\d.]+:(\d+\.?\d*))*$/.test(env)) {
      throw new Error(`Invalid CITIES_REPOSITORY_FUSE_INDEXES syntax: ${env}`);
    }
    return env.split(',').map((index) => {
      const [name, weight] = index.split(':');
      return { name, weight: parseInt(weight) };
    });
  }

  return {
    indexes: getIndexes(),
    textDistance: c.get<number>('CITIES_REPOSITORY_FUSE_TEXT_DISTANCE', 5),
    scoreThreshold: c.get<number>(
      'CITIES_REPOSITORY_FUSE_SCORE_THRESHOLD',
      0.6,
    ),
  };
}
