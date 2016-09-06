import complete from './complete'
import geoDistFromCities from './redisGeoDistFromCities'
import scoring from './suggestScoring'

const DEFAULT_OPTIONS = {
  minPopulation: 5000,
  scoring: {
    lexico: 0.2,
    distance: 0.65,
    population: 0.35
  }
}

const filterByPopulationAbove = (limit) => (cities = []) => {
  return cities.filter((city) => {
    const pop = parseInt(city.population)
    return pop > limit
  })
}

const compareScore = (a, b) => {
  return parseFloat(b.score) - parseFloat(a.score)
}

export default (q, long, lat, options = DEFAULT_OPTIONS) => {
  const defineScoring = scoring(options.scoring)

  return complete(q)
    .then(filterByPopulationAbove(options.minPopulation))
    .then((cities) => {
      if (long && lat) {
        return geoDistFromCities(long, lat, cities)
      }
      return cities
    })
    .then(defineScoring)
    .then((cities) => cities.sort(compareScore))
}
