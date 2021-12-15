import {City} from "../repository/city";
import {cityRepository} from "../repository/city"

const deburr = require('lodash/deburr');

class CityController {

  /**
   * Return all cities
   */
  getAllCities = async () => {
    return cityRepository.getCities()
  }

  /**
   * Return all cities that contains the string passed in parameter.
   * Note that we tested the string with lower case only, and without any accent with the help of
   * lodash/deburr
   *
   * @param {string} substring Substring that needs to match the city name
   * @returns {Array<City>} All cities that matches the substring
   */
  getCitiesLikeName = (substring: string) => {
    return cityRepository.getCities().filter((city) =>
        deburr(city.name).toLowerCase().includes(deburr(substring).toLowerCase())
    )
  }

  sortByDistance = (cities: Array<City>, lat: number, lon: number) => {
    const citiesDistances = cities.map((city) => {
      return {
        ...city,
        distance: this.distance(city.lat, city.long, lat, lon, 'K')
      }
    })

    return citiesDistances.sort((a, b) =>
        a.distance - b.distance
    )
  }

  distance(lat1, lon1, lat2, lon2, unit) {
    var radlat1 = Math.PI * lat1 / 180
    var radlat2 = Math.PI * lat2 / 180
    var radlon1 = Math.PI * lon1 / 180
    var radlon2 = Math.PI * lon2 / 180
    var theta = lon1 - lon2
    var radtheta = Math.PI * theta / 180
    var dist = Math.sin(radlat1) * Math.sin(radlat2) + Math.cos(radlat1) * Math.cos(radlat2) * Math.cos(radtheta);
    dist = Math.acos(dist)
    dist = dist * 180 / Math.PI
    dist = dist * 60 * 1.1515
    if (unit == "K") {
      dist = dist * 1.609344
    }
    if (unit == "N") {
      dist = dist * 0.8684
    }
    return dist
  }

  /**
   * Convert a city to a dto to express.
   * @param {Array<City>} values
   */
  toDto = (values: City[]) =>
      values.map((city) => {
        return {
          name: city.name,
          latitude: city.lat,
          longitude: city.long,
          score: 'todo'
        }
      })
}

export default new CityController()
