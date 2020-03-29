import { expect } from 'chai';

import { DistanceHelper } from '../../src/utils';

describe('distance.helper', () => {
  it('convert_degree_to_rad', () => {
    expect(DistanceHelper.rad(180)).to.equal(Math.PI);
    expect(DistanceHelper.rad(-90)).to.equal(-1 * Math.PI / 2);
  });
  it('#round2Decimal', () => {
    expect(DistanceHelper.round2Decimal(10.1232)).to.equal(10.12);
    expect(DistanceHelper.round2Decimal(10.1)).to.equal(10.10);
  });
});
