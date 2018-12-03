let cache = {};

// set a value
function set(key, value, serializer = JSON.stringify) {
  cache[key] = serializer(value);
}

// get a value
function get(key, deserializer = JSON.parse) {
  const value = cache[key];
  return {
    serialized: value,
    json: value !== undefined ? deserializer(value) : value
  };
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
function fromCacheOr(key, operation, serializer = JSON.stringify) {
  const cached = get(key);
  if (cached.json === undefined) {
    const result = operation();
    set(key, result);
    return {
      serialized: serializer(result),
      json: result
    };
  }
  return cached;
}

module.exports = {
  cache,
  fromCacheOr,
  get,
  set,
  clear
};
