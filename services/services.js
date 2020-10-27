const haversine = require('haversine')
const stringSimilarity = require('string-similarity')

const utils = require('./utils')

class Services {
  constructor(datasource) {
    this.ds = datasource
  }

  computeScore(city, normalizedQuery, latitude, longitude) {
    const SCALES = {
      distance: // in kilometres
        [
          { threshold: 0, score: +.4 },
          { threshold: 50, score: +.3 },
          { threshold: 100, score: +.2 },
          { threshold: 250, score: +.1 },
          { threshold: 500, score: 0 },
          { threshold: 1000, score: -.1 },
          { threshold: 2000, score: -.2 },
          { threshold: 3000, score: -.3 },
        ],
      population:
        [
          { threshold: 0, score: -.4 },
          { threshold: 25000, score: -.3 },
          { threshold: 50000, score: -.2 },
          { threshold: 100000, score: -.1 },
          { threshold: 250000, score: 0 },
          { threshold: 500000, score: +.1 },
          { threshold: 1000000, score: +.2 },
          { threshold: 2000000, score: +.3 },
          { threshold: 3000000, score: +.4 },
        ],
    }

    const searchScore = (city, normalizedQuery) => {
      return stringSimilarity.compareTwoStrings(city.normalizedName, normalizedQuery)
    }

    const distanceModifier = (city, latitude, longitude) => {
      if (latitude !== undefined && longitude !== undefined) {
        return getModifier(haversine(city, { latitude, longitude }), 'distance')
      } else {
        return 0
      }
    }

    const populationModifier = (city) => {
      return getModifier(city.population || 0, 'population')
    }

    const getModifier = (value, key) => {
      const scale = SCALES[key]

      for (let i = 0; i < scale.length; i++) {
        const nextThreshold = scale[i + 1] ? scale[i + 1].threshold : Infinity
        if (value >= scale[i].threshold && value < nextThreshold) {
          return scale[i].score
        }
      }
    }

    const baseScore = searchScore(city, normalizedQuery)
    const modifiers = distanceModifier(city, latitude, longitude) + populationModifier(city)
    return baseScore * (1 + modifiers)
  }

  getMatches(normalizedQuery) {
    const terms = normalizedQuery.split(' ')
    return terms.reduce((candidates, term) => {
      return candidates.filter(city => city.index.indexOf(` ${term}`) !== -1)
    }, this.ds.getCities())
  }

  getSuggestions(query, latitude, longitude) {
    const normalizedQuery = utils.normalizeString(query)

    const matches = this.getMatches(normalizedQuery)

    const results = matches.map(city => {
      return {
        name: city.getDisplayName(),
        latitude: city.latitude,
        longitude: city.longitude,
        score: this.computeScore(city, normalizedQuery, latitude, longitude),
      }
    })

    // Normalize scores
    const maxScore = results.reduce((memo, curr) => {
      return curr.score > memo ? curr.score : memo
    }, 1)
    results.forEach(result => {
      result.score = Number((result.score / maxScore).toFixed(1))
    })

    return utils.sort(results, 'score', 'desc')
  }
}

module.exports = Services