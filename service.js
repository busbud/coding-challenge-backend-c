import { cities } from './cities.js'

export class Service {
  constructor() {}

  async getSuggestions(req, resp) {
    const partial_name = req.query.q
    const partial_matcher = new RegExp(`${partial_name}`, 'i')
    const s = cities.c.filter(city => {
      if (city != undefined) {
        return partial_matcher.test(city.name)
      } else {
        return false
      }
    }).map(city => {
      let n
      let lat
      let lon
      n = city.name
      lat = city.lat
      lon = city['long']
      if (!partial_matcher.test(n)) {
        // cities that don't match the name should be skipped, jump to the next
        // step of the map iteration
        // TODO
        return {}
      }
      return {
        name: n,
        latitude: lat,
        longitude: lon,
        score: "0.5"
      }
    })
    return { suggestions: s }
  }
}
