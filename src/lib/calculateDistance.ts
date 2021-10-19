// Calculates the distance between two informed points using geolib library (possible to include elevation in the future)
// https://www.npmjs.com/package/geolib
// Decided to go with geolib insteadh of haversine_formula (https://en.wikipedia.org/wiki/Haversine_formula)
// since it seems to be larger, more performant and more mature project

import { getDistance } from 'geolib'

export function calculateDistance(cityLat: string, cityLon: string, paramLat: string, paramLon: string) {
    const cityPoint = {latitude: Number(cityLat), longitude: Number(cityLon)}
    const informedPoint = {latitude: Number(paramLat), longitude: Number(paramLon)}

    return getDistance(cityPoint, informedPoint)
}
