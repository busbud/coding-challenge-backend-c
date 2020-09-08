import { expect } from 'chai';
import { notValidNumber } from '../../../src/utils';

describe('General utils', () => {
  describe('Utils notValidNumber', () => {
    it('should return false with valid number', () => {
      expect(notValidNumber('-75.009')).to.be.false;
      expect(notValidNumber('1')).to.be.false;
    });

    it('should return true with valid number', () => {
      expect(notValidNumber('A-75.009abc')).to.be.true;
      expect(notValidNumber('notAValidNumber')).to.be.true;
    });
  });
});
