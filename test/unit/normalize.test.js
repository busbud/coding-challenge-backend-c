import { expect } from 'chai'
import normalize from '../../src/lib/normalize'

describe('Normalize', () => {
  const data = [1, 5, 3, 9]

  it('returns normalize scale beetwen [0-1]', () => {
    const results = normalize(data)
    expect(results).to.be.deep.equal([0, 0.5, 0.25, 1])
  })

  it('returns normalize scale beetwen [0-10]', () => {
    const results = normalize(data, {
      start: 0,
      end: 10
    })

    expect(results).to.be.deep.equal([0, 5, 2.5, 10])
  })
})
