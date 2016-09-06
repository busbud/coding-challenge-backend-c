import { expect } from 'chai'
import suggestScoring from '../../src/services/cities/suggestScoring'

describe('Suggest scoring', () => {
  const data = []

  data.push({
    name: 'city1',
    population: 2,
    distance: {
      value: 1000
    }
  })

  data.push({
    name: 'city2',
    population: 2,
    distance: {
      value: 20
    }
  })

  data.push({
    name: 'city3',
    population: 20,
    distance: {
      value: 400
    }
  })

  it('contains a score', () => {
    const weights = {
      index: 0.7,
      population: 0.2,
      distance: 0.5
    }
    const results = suggestScoring(weights)(data)

    expect(results).to.satisfy((data) => {
      return data.every((d) => d.hasOwnProperty('score'))
    })
  })
})
