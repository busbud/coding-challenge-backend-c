import { expect } from 'chai'
import zscore, { mean, deviation } from '../../src/lib/zscore'

describe('Zscore', () => {
  const data = [2, 4, 4, 4, 5, 5, 7, 9]

  describe('mean', () => {
    it('returns valid a mean', () => {
      const result = mean(data)
      expect(result).to.be.equal(5)
    })
  })

  describe('deviation', () => {
    it('returns valid a deviation', () => {
      const result = deviation(data)
      expect(result).to.be.equal(2)
    })
  })

  it('returns a valid zscore', () => {
    const results = zscore(data)
    expect(results).to.be.deep.equal(
      [-1.5, -0.5, -0.5, -0.5, 0, 0, 1, 2]
    )
  })
})
