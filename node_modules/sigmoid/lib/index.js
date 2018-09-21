
/**
 * Expose `sigmoid`.
 */

module.exports = sigmoid;

/**
 * sigmoid.
 *
 * 	- Non-linear, continuous, and differentiable logistic function.
 *
 * @param {Number} z
 */

function sigmoid(z) {
  return 1 / (1 + Math.exp(-z));
}
