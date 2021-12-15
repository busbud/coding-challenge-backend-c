import {City} from "../repository/city";
import {cityRepository} from "../repository/city"
import stringSimilarity from 'string-similarity';
import deburr from 'lodash/deburr';

const POPULATION_MIN: number = 5000

class CityController {

  /**
   * Return all cities
   */
  getAllCities = async () => {
    return cityRepository.getCities()
  }

  /**
   * Return all cities with their score matching the corresponding substring
   * Note that we tested the string with lower case only, and without any accent with the help of
   * lodash/deburr
   *
   * @param {string} substring Substring that needs to match the city name
   * @returns {Array<City>} All cities that matches the substring
   */
  findAllCitiesScores = (substring: string, lat?: number, lon?: number) => {
    return cityRepository.getCities()
        .filter((city) =>  // Filter by population > 5000 and those that actually have the substring inside their name
            city.population > POPULATION_MIN && deburr(city.name).toLowerCase().includes(deburr(substring).toLowerCase())
            || deburr(city.alt_name).toLowerCase().includes(deburr(substring).toLowerCase())
        )
        .map((city) => { // Calculate scores
          return {
            ...city,
            name: `${city.name}, ${city.country}`,
            score: stringSimilarity.compareTwoStrings(city.name, substring),
            distance: this.distance(city.lat, city.long, lat, lon, 'K')
          }
        })// Sort by scores descending.
        .sort((a, b) =>
            b.score - a.score || a.distance - b.distance
        )

  }

  /**
   * Find the distance between two point with lat lon.
   * Source : https://www.geodatasource.com/developers/javascript
   * @param lat1
   * @param lon1
   * @param lat2
   * @param lon2
   * @param unit
   */
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
          score: city.score
        }
      })
}

export default new CityController()
