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
