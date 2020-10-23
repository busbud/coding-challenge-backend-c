const haversineDistance = require('haversine-distance')
const diacritics = require('diacritics')

class Services {
  constructor(ds) {
    this.ds = ds
  }

  computeScore(city, search, latitude, longitude) {
    const WEIGHTS = { search: 0.5, distance: 0.3, population: 0.2 } // Completely arbitrary
    const SCALES = {
      search: // difference in length
        [
          { threshold: 0, score: 1 },
          { threshold: 1, score: 0.7 },
          { threshold: 4, score: 0.3 },
          { threshold: 10, score: 0 },
        ],
      distance: // in kilometres
        [
          { threshold: 0, score: 1 },
          { threshold: 100, score: 0.7 },
          { threshold: 300, score: 0.3 },
          { threshold: 1000, score: 0 },
        ],
      population:
        [
          { threshold: 0, score: 0 },
          { threshold: 10000, score: 0.1 },
          { threshold: 100000, score: 0.3 },
          { threshold: 500000, score: 0.5 },
          { threshold: 1000000, score: 0.7 },
          { threshold: 3000000, score: 1 },
        ],
    }

    const searchScore = (city, search) => {
      return toScore(city.name.length - search.length, 'search')
    }

    const distanceScore = (city, latitude, longitude) => {
      if (latitude && longitude && !isNaN(latitude) && !isNaN(longitude)) {
        return toScore(haversineDistance(city, { latitude, longitude }) / 1000, 'distance')
      } else {
        return 0
      }
    }

    const populationScore = (city) => {
      return toScore(city.population || 0, 'population')
    }

    const normalize = score => {
      return Math.min(Math.max(score, 0), 1)
    }

    const toScore = (value, key) => {
      const scale = SCALES[key]
      const weight = WEIGHTS[key]

      for (let i = 0; i < scale.length; i++) {
        const nextThreshold = scale[i + 1] ? scale[i + 1].threshold : Infinity
        if (value >= scale[i].threshold && value < nextThreshold) {
          return normalize(scale[i].score) * weight
        }
      }
    }

    return Number((searchScore(city, search)
      + distanceScore(city, latitude, longitude)
      + populationScore(city)).toFixed(1))
  }

  getSuggestions(search, latitude, longitude) {
    const matches = this.ds.getCities().filter(x => x.asciiName.startsWith(diacritics.remove(search)))

    let maxDistance = 0

    const results = matches.map(city => {
      return {
        name: city.getDisplayName(),
        latitude: city.latitude,
        longitude: city.longitude,
        score: this.computeScore(city, search, latitude, longitude),
      }
    })

    return results.sort((a, b) => {
      if (a.score === b.score) return 0
      return a.score < b.score ? 1 : -1
    })
  }
}

module.exports = Services