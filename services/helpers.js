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

export const getLocationBetweenTwoPoints = (
  node,
  incomingLatitude,
  incomingLongitude
) => {
  const {
    information: { latitude, longitude },
  } = node;
  const R = 6371e3; // metres
  const φ1 = (incomingLatitude * Math.PI) / 180;
  const φ2 = (latitude * Math.PI) / 180;
  const Δφ = ((latitude - incomingLatitude) * Math.PI) / 180;
  const Δλ = ((longitude - incomingLongitude) * Math.PI) / 180;

  const a =
    Math.sin(Δφ / 2) * Math.sin(Δφ / 2) +
    Math.cos(φ1) * Math.cos(φ2) * Math.sin(Δλ / 2) * Math.sin(Δλ / 2);
  const c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));

  const d = R * c; // in metres
  return d;
};
