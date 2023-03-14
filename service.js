import { cities } from './cities.js'

export class Service {
  constructor() {}

  async getSuggestions(req, resp) {
    const result = { suggestions: cities.c.map(x => {
      let n
      let lat
      let lon
      if (x != undefined) {
        n = x.name
        lat = x.lat
        lon = x['long']
      } else {
        n = "nil"
      }
      return {
        name: n,
        latitude: lat,
        longitude: lon,
        score: "0.5"
      }
    })}
    return result
  }
}
