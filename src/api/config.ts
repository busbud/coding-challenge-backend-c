import dotenv from 'dotenv';
import { resolve, dirname } from 'path';
import { findUpSync } from 'find-up';

// Used to retrieve an environment variable or fallback value if not defined.
function get(name: string, fallback: string) {
  return process.env[name] || fallback;
}

// Environment configuration
export const PROJECT_ROOT = dirname(findUpSync('package.json') || '') || __dirname;

dotenv.config({
  path: resolve(exports.PROJECT_ROOT, '.env')
});

// Server configuration
export const HOST = get('HOST', '127.0.0.1');

export const PORT = parseInt(get('PORT', '3000'), 10);

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

// Return any configuration errors - e.g. if any required environment values are not defined.
export function getConfigErrors() {
  const errors = [];
  if (!exports.REDIS_URL) {
    errors.push('REDIS connection parameters must be specified. Check your environment.');
  }
  return errors;
}
