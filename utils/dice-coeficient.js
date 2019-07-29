/**
 * Scores the difference between 2 strings from 0 to 1
 * @param {String} l first string
 * @param {String} r second string
 * @returns {Float} score
 *
 * https://en.wikibooks.org/wiki/Algorithm_Implementation/Strings/Dice%27s_coefficient#Javascript
 */
module.exports = function (l, r) {
  if (l.length < 2 || r.length < 2) return 0;

  let lBigrams = new Map();
  for (let i = 0; i < l.length - 1; i++) {
    const bigram = l.substr(i, 2);
    const count = lBigrams.has(bigram)
      ? lBigrams.get(bigram) + 1
      : 1;

    lBigrams.set(bigram, count);
  };

  let intersectionSize = 0;
  for (let i = 0; i < r.length - 1; i++) {
    const bigram = r.substr(i, 2);
    const count = lBigrams.has(bigram)
      ? lBigrams.get(bigram)
      : 0;

    if (count > 0) {
      lBigrams.set(bigram, count - 1);
      intersectionSize++;
    }
  }

  return (2.0 * intersectionSize) / (l.length + r.length - 2);
};