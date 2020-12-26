import { LocationConfig } from './location.service';
import { ConfigService } from '@nestjs/config';

export default function (c: ConfigService): LocationConfig {
  return {
    geohashPrecision: c.get<number>('LOCATION_GEOHASH_PRECISION', 3),
  };
}
