import { expect } from 'chai';
import { calculateScore } from "./calculateScore"
require("dotenv").config()

describe('calculateScore', () => {
  
  describe('when no distance informed', () => {
    it('should return word score', () => {
        expect(calculateScore(1,0)).to.equal(1)
    });
  })

  describe('with distance informed', () => {
    it('should decrease score by 0.1', () => {
        expect(calculateScore(1,100001)).to.equal(0.9)
    });

    it('should never go lower than 0', () => {
      expect(calculateScore(0,100001)).greaterThanOrEqual(0)
    });

  })

})
