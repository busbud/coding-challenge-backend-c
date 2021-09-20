import dotenv from 'dotenv';
import { resolve, dirname } from 'path';
import findUp from 'find-up';

// Retrieve an environment variable or fallback value if not defined.
function get(name: string, fallback: string) {
  return process.env[name] || fallback;
}

// Environment configuration
export const REPO_ROOT_DIR = dirname(findUp.sync('package.json') || '') || __dirname;

dotenv.config({
  path: resolve(REPO_ROOT_DIR, '.env')
});

// Server configuration
export const PORT = parseInt(get('PORT', '3000'), 10);
export const HOST = get('HOST', '127.0.0.1');
export const REDIS_URL = getRedisUrl();

// Return the specified connection URL for the Redis instance.
export function getRedisUrl() {
  const host = get('REDIS_HOST', '');
  const port = get('REDIS_PORT', '');
  const user = get('REDIS_USER', '');
  const pwd = get('REDIS_PWD', '');
  if ([host, port, user, pwd].every(value => !!value)) {
    return `redis://${user}:${pwd}@${host}:${port}`;
  }
    else {
      // Fallback to full URL if it's specified.
      return get('REDIS_URL', '');
    }
}

// Data load configuration
export const DATA_FILE_PATH = get('DATA_FILE_PATH', 'data/cities_canada-usa.tsv')

// These indices describe which columns to use in the TSV data file.
export const DATA_ID_INDEX = parseInt(get('DATA_ID_INDEX', '0'), 10);
export const DATA_NAME_INDEX = parseInt(get('DATA_NAME_INDEX', '1'), 10);
export const DATA_ASCII_NAME_INDEX = parseInt(get('DATA_ASCII_NAME_INDEX', '2'), 10);
export const DATA_LAT_INDEX = parseInt(get('DATA_LAT_INDEX', '4'), 10);
export const DATA_LONG_INDEX = parseInt(get('DATA_LONG_INDEX', '5'), 10);
export const DATA_COUNTRY_INDEX = parseInt(get('DATA_COUNTRY_INDEX', '8'), 10);
export const DATA_TERR_INDEX = parseInt(get('DATA_TERR_INDEX', '10'), 10);
export const DATA_POPULATION_INDEX = parseInt(get('DATA_POPULATION_INDEX', '14'), 10);

// Threshold for city population 
export const DATA_CITY_MIN_POP = parseInt(get('DATA_CITY_MIN_POP', '5000'), 10);

// Return any configuration errors - e.g. if any required environment values are not defined and we shouldn't start the server.
export function getConfigErrors() {
  const errors = [];
  if (!REDIS_URL) {
    errors.push('REDIS connection parameters must be specified. Check your environment.');
  }
  return errors;
}
