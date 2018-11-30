/**
 * a simple curry implementation
 * @param {*} fn
 */
function curry(fn, initialArgs = []) {
  // the target arguments count
  const argsToAchieve = fn.length;
  // return a callable
  return (...args) => {
    // if the propagated args from the upper context + the current arguments
    // are less the target number of arguments, curry recursively
    const aggregatedArgs = [...initialArgs, ...args];
    if (aggregatedArgs.length < argsToAchieve) return curry(fn, aggregatedArgs);
    // else apply the args to the function
    return fn(...aggregatedArgs);
  };
}

module.exports = {
  curry
};
