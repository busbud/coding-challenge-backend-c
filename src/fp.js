//List of abstract functions used to facilitate functional programing (fp).

/**
 *
 * Pipe all streams returned by the callback functions.
 *
 * Usage:
 *
 * composeStream(
 *  streamFilter(fn),
 *  streamMap(fn)
 * )(stream)
 *
 * @param  {...Function} fns
 * @returns { Stream }
 */
const composeStream = (...fns) => {
  return stream => {
    return fns.reduce((acc, fn) => {
      return acc.pipe(fn);
    }, stream);
  };
};

/**
 * Reduce an object by applying to the initial value all the
 * fns pass as argurment.
 *
 * Usage:
 *
 * composeObjectReduce(
 *  (acc, obj) => acc + obj.value,
 *  fn2(params),
 *  fn3
 * )(obj)(inititalValue || null)
 *
 *
 * @param  {...Function} fns
 */
const composeObjectReduce = (...fns) => {
  return object => {
    return initialValue => {
      return fns.reduce((acc, fn) => {
        return fn(acc, object);
      }, initialValue || null);
    };
  };
};

/**
 * Returns the array successively modified by the function pass as parameters.
 *
 * Usage:
 *
 * composeMap(
 *  (row) => {row.a+=1; return row},
 *  fn2(param),
 *  fn3()
 * )(array)
 *
 * @param  {...Function} fns
 */
const composeMap = (...fns) => {
  return array => {
    return array.map(row => {
      return fns.reduce((acc, fn) => {
        return fn(acc);
      }, row);
    });
  };
};

exports.composeStream = composeStream;
exports.composeObjectReduce = composeObjectReduce;
exports.composeMap = composeMap;
