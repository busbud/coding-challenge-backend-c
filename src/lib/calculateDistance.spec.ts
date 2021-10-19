import { expect } from 'chai';
import { calculateDistance } from "./calculateDistance"
require("dotenv").config()

// Unit tests values taken from https://github.com/manuelbieh/geolib

describe('calculateDistance', () => {
  
  describe('when two points informed', () => {
    it('should calculate the distance between any two points', () => {
      expect(calculateDistance("52.518611", "13.408056", "51.519475", "7.46694444")).to.equal(421786)
    });

    it('should return 0 if two identical points are given', () => {
        expect(calculateDistance("52.518611", "13.408056", "52.518611", "13.408056")).to.equal(0)
    });
  })

})
