let cache = {};

// set a value
function set(key, value) {
  cache[key] = JSON.stringify(value);
}

// get a value
function get(key) {
  return cache[key];
}

//  clear the cache
function clear(key, value) {
  cache = {};
}

/**
 * retrieves data from cache if the keys exists, otherwise, get it from the operation
 * @param {*} key
 * @param {*} operation
 */
function fromCacheOr(key, operation) {
  const cached = get(key);
  if (!cached) {
    const result = operation();
    set(key, result);
    return result;
  }
  return cached;
}

module.exports = {
  fromCacheOr,
  get,
  set,
  clear
};
