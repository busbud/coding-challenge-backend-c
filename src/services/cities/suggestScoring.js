import zscore from '../../lib/zscore'
import normalize from '../../lib/normalize'
import scoring from './scoring'

export default (weights) => (cities = []) => {
  const calculateScore = (city, index) => {
    const score = Object.entries(scoring)
      .reduce((s, [prop, fn]) => {
        s[prop] = fn(city[prop], index, cities) * (weights[prop] || 0)
        return s
      }, {})

    score.total = Object.keys(score).reduce((t, k) => {
      t += score[k]
      return t
    }, 0)

    return score
  }

  const scores = cities.map((c, i) => calculateScore(c, i))
  const zScores = zscore(scores.map((c) => c.total))
  const min = Math.min(...zScores) - 1
  const max = Math.max(...zScores) + 1
  const nScores = zScores.map((z) => normalize({
    value: z,
    min,
    max
  }))

  return nScores.map((score, index) => {
    return Object.assign({
      score: score
    }, cities[index])
  })
}
