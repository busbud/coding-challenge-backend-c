const md5 = require('md5');

/**
 * Utility function for building hashes out of a request's query parameters for cache keys
 */
export default function hashify(...args) {
  return md5(args.join(''));
}
