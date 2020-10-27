const haversine = require('haversine')
const stringSimilarity = require('string-similarity')

const utils = require('./utils')

class Services {
  constructor(datasource) {
    this.ds = datasource
  }

  computeScore(city, normalizedQuery, latitude, longitude) {
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
      return Math.min(Math.max(similarityScore, .1), 1)
    }

    const getDistanceModifier = (city, latitude, longitude) => {
      if (latitude !== undefined && longitude !== undefined) {
        return getModifier(haversine(city, { latitude, longitude }), DISTANCE_MODIFIERS)
      } else {
        return 0
      }
    }

    const getPopulationModifier = (city) => {
      return getModifier(city.population || 0, POPULATION_MODIFIERS)
    }

    const getModifier = (value, modifiers) => {
      for (let i = 0; i < modifiers.length; i++) {
        const nextThreshold = modifiers[i + 1] ? modifiers[i + 1].threshold : Infinity
        if (value >= modifiers[i].threshold && value < nextThreshold) {
          return modifiers[i].score
        }
      }
    }

    const searchScore = getSearchScore(city, normalizedQuery)
    const modifiers = getDistanceModifier(city, latitude, longitude) + getPopulationModifier(city)

    // Apply modifiers relative to string similarity
    const score = searchScore * (1 + modifiers)

    // Perfect score is reserved for perfect matches only
    return searchScore === 1 ? score : Math.min(score, .9)
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