'use strict';

/**
 * Combine multiple scoring functions with a weight.
 *
 * Each item in the specification array is an array, where the first item is
 * the scoring function, and the second item is a weight.
 *
 *     engine([
 *       [length, .4],
 *       [distance, .6]
 *     ])
 */
export default spec => (query, items) => {
  const scorings = spec.map(([f, weight]) => [f(query, items), weight]);

  return item =>
    scorings.reduce(
      (score, [f, weight]) =>
        score + f(item) * weight,
      0
    );
};
