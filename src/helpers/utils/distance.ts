export function degreesToRadians(degrees: number) {
  return degrees * (Math.PI / 180);
}

// formula adapted from https://stackoverflow.com/questions/27928/calculate-distance-between-two-latitude-longitude-points-haversine-formula
export function calculateDistanceInKm(
  latitude1: number,
  longitude1: number,
  latitude2: number,
  longitude2: number
) {
  const R = 6371; // radius of the earth in km

  const distanceLatitude = degreesToRadians(latitude2 - latitude1);
  const distanceLongitude = degreesToRadians(longitude2 - longitude1);

  const a =
    Math.sin(distanceLatitude / 2) * Math.sin(distanceLatitude / 2) +
    Math.cos(degreesToRadians(latitude1)) *
      Math.cos(degreesToRadians(latitude2)) *
      Math.sin(distanceLongitude / 2) *
      Math.sin(distanceLongitude / 2);

  const c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));
  const distance = R * c; // distance in km

  return distance;
}
