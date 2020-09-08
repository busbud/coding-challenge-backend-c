import { distanceBetweenTwoPoints, getGeoScore } from '../../../src/utils/distance.util';
import { expect } from 'chai';

describe('Distance Calculation Util', () => {
  describe('when getting distance between two points', () => {
    it('should calculate the correct distance between two points in meters', () => {
      expect(distanceBetweenTwoPoints({ lat: -74.0, long: 73.0 }, { lat: 74.0, long: -73.0 })).to.equal(
        14943635.632012712,
      );
    });
  });

  describe('when getting the geo score from two points', () => {
    it('should calculate the correct score', () => {
      expect(getGeoScore({ lat: -74.0, long: 73.0 }, { lat: 74.0, long: -73.0 })).to.equal(0.25338142250855356);
    });
  });
});
