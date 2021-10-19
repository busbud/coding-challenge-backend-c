import { expect } from 'chai';
import { calculateWordScore } from "lib/calculateWordScore"
require("dotenv").config()

describe('calculateWordScore', () => {
  
  describe('with same word', () => {
    it('should return score 1', () => {
        expect(calculateWordScore("UNIT_TEST","UNIT_TEST")).to.equal(1)
    });
  })

  describe('with different words', () => {
    it('should return score less than 1', () => {
        expect(calculateWordScore("UNIT_","UNIT_TEST")).to.lessThan(1)
    });
  })

})
