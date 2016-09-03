const DEFAULT_WEIGHTS = {
  distance: 0.5,
  length: 0.2,
  population: 0.3
}

//@todo
export default (weights = DEFAULT_WEIGHTS) => (cities = []) => {
  const scoring = (city, index) => {
    let score = 0
    return 0.5 * score
  }

  return cities.map((c, i) => {
    c.score = scoring(c, i)
    return c
  })
}