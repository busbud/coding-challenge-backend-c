const haversine = require('haversine')
const stringSimilarity = require('string-similarity')

const utils = require('./utils')

class Services {
  constructor(datasource) {
    this.ds = datasource
  }

  computeScore(city, normalizedSearch, latitude, longitude) {
    const WEIGHTS = { search: 0.5, distance: 0.3, population: 0.2 } // Completely arbitrary
    const SCALES = {
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
          { threshold: 500000, score: 0.7 },
          { threshold: 1000000, score: 1 },
        ],
    }

    const searchScore = (city, normalizedSearch) => {
      return stringSimilarity.compareTwoStrings(city.normalizedName, normalizedSearch) * WEIGHTS.search
    }

    const distanceScore = (city, latitude, longitude) => {
      if (latitude !== undefined && longitude !== undefined) {
        return toScore(haversine(city, { latitude, longitude }), 'distance')
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

    return Number((searchScore(city, normalizedSearch)
      + distanceScore(city, latitude, longitude)
      + populationScore(city)).toFixed(1))
  }

  getSuggestions(search, latitude, longitude) {
    const normalizedSearch = ` ${utils.normalizeString(search)}`

    const matches = this.ds.getCities().filter(city => city.index.indexOf(normalizedSearch) !== -1)

    let maxDistance = 0

    const results = matches.map(city => {
      return {
        name: city.getDisplayName(),
        latitude: city.latitude,
        longitude: city.longitude,
        score: this.computeScore(city, normalizedSearch, latitude, longitude),
      }
    })

    return utils.sort(results, 'score', 'desc')
  }
}

module.exports = Services