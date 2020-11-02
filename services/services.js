const haversine = require('haversine')
const stringSimilarity = require('string-similarity')

const utils = require('./utils')

class Services {
  constructor(datasource) {
    this.ds = datasource
  }

  computeScore(city, normalizedQuery, latitude, longitude, nbMatches) {
    const DISTANCE_WEIGHT = 1
    const DISTANCE_MODIFIERS = [ // in kilometers
      { threshold: 0, score: +.4 },
      { threshold: 25, score: +.3 },
      { threshold: 50, score: +.2 },
      { threshold: 100, score: +.1 },
      { threshold: 200, score: 0 },
      { threshold: 400, score: -.1 },
      { threshold: 800, score: -.2 },
      { threshold: 1600, score: -.3 },
      { threshold: 3200, score: -.4 },
    ]

    const POPULATION_WEIGHT = 1
    const POPULATION_MODIFIERS = [
      { threshold: 0, score: -.4 },
      { threshold: 25000, score: -.3 },
      { threshold: 50000, score: -.2 },
      { threshold: 100000, score: -.1 },
      { threshold: 250000, score: 0 },
      { threshold: 500000, score: +.1 },
      { threshold: 1000000, score: +.2 },
      { threshold: 2000000, score: +.3 },
      { threshold: 3000000, score: +.4 },
    ]

    const getSearchScore = (city, normalizedQuery) => {
      const similarityScore = stringSimilarity.compareTwoStrings(city.normalizedName, normalizedQuery)
      return Math.max(similarityScore, .1)
    }

    const getDistanceModifier = (city, latitude, longitude) => {
      if (latitude !== undefined && longitude !== undefined) {
        return getModifier(haversine(city, { latitude, longitude }), DISTANCE_MODIFIERS) * DISTANCE_WEIGHT
      } else {
        return 0
      }
    }

    const getPopulationModifier = (city) => {
      return getModifier(city.population || 0, POPULATION_MODIFIERS) * POPULATION_WEIGHT
    }

    const getModifier = (value, modifiers) => {
      for (let i = 0; i < modifiers.length; i++) {
        const nextThreshold = modifiers[i + 1] ? modifiers[i + 1].threshold : Infinity
        if (value >= modifiers[i].threshold && value < nextThreshold) {
          return modifiers[i].score
        }
      }
    }

    // If there is a single result, we can have certainty
    if (nbMatches === 1) return 1

    const searchScore = getSearchScore(city, normalizedQuery)
    const modifiers = getDistanceModifier(city, latitude, longitude) + getPopulationModifier(city)

    // Apply modifiers relative to string similarity
    const score = searchScore * (1 + modifiers)

    return Number(Math.max(Math.min(score, .999), 0).toFixed(3))
  }

  getMatches(normalizedQuery) {
    // TODO: this could be improved by making sure matches are on consecutive words in order of appearance
    const terms = normalizedQuery.split(' ')
    let initialCandidates = this.ds.getCities().filter(city => city.normalizedName.startsWith(terms[0]))
    return terms.slice(1).reduce((candidates, term) => {
      return candidates.filter(city => city.normalizedName.indexOf(` ${term}`) !== -1)
    }, initialCandidates)
  }

  getSuggestions(query, latitude, longitude) {
    const normalizedQuery = utils.normalizeString(query)

    const matches = this.getMatches(normalizedQuery)

    const results = matches.map(city => {
      return {
        name: city.getDisplayName(),
        latitude: city.latitude,
        longitude: city.longitude,
        score: this.computeScore(city, normalizedQuery, latitude, longitude, matches.length),
      }
    })

    return utils.sort(results, 'score', 'desc')
  }
}

module.exports = Services