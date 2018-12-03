/**
 * return the number with at most 2 decimals
 * @param {number} number
 */
function twoDecimalsAtMost(number) {
  return +Number(number).toFixed(2);
}

module.exports = {
  twoDecimalsAtMost
};
