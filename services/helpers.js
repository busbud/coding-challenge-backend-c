/**
 * This helper does all the checking for you to see if it's the last iteration from the array.
 * @param {array} arr
 * @param {number} i iterator
 * @returns boolean
 */
export const isLastItemFromArray = (arr, i) => {
  if (!arr) return true;
  if (!arr.length) return true;
  return arr.length - 1 === i;
};
