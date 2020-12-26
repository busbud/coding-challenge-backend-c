import { CitiesSeederConfiguration } from './cities.seeder';

export default function (): CitiesSeederConfiguration {
  return {
    dataPath: process.env.CITIES_TSV_PATH || 'data/cities_canada-usa.tsv',
  };
}
