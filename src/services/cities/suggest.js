import complete from './complete'
import geoDistFromCities from './redisGeoDistFromCities'
import suggestScoring from './suggestScoring'

const DEFAULT_OPTIONS = {
  population: 5000,
  scoring: {
    distance: 0.5,
    length: 0.2,
    population: 0.3
  }
}

const filterByPopulationAbove = (limit) => (cities = []) => {
  return cities.filter((city) => {
    const pop = parseInt(city.population)
    return pop > limit
  })
}

const compareScore = (a, b) => {
  if (a.score && b.score) {
    return b.score - a.score
  }

  if (a.name > b.name) {
    return 1
  }

  if (a.name < b.name) {
    return -1
  }
}

export default (q, long, lat, options = DEFAULT_OPTIONS) => {
  return complete(q)
    .then(filterByPopulationAbove(options.population))
    .then((cities) => {
      if (long && lat) {
        return geoDistFromCities(long, lat, cities)
      }
      return cities
    })
    .then(suggestScoring(options.scoring))
    .then((cities) => cities.sort(compareScore))
}