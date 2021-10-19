//https://en.wikipedia.org/wiki/Haversine_formula
//https://www.npmjs.com/package/geolib

import { getDistance } from 'geolib'

export function calculateDistance(cityLat: string, cityLon: string, paramLat: string, paramLon: string) {
    const cityPoint = {latitude: Number(cityLat), longitude: Number(cityLon)}
    const informedPoint = {latitude: Number(paramLat), longitude: Number(paramLon)}

    return getDistance(cityPoint, informedPoint)
}
