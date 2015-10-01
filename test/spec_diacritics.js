const expect = require('chai').expect;
const rmDiac = require('../src/helpers/diacritics');

describe('the diacritic removal tool', () => {

  it('should convert special characters', () => {
    expect(rmDiac('Montréal')).to.equal('Montreal');
    expect(rmDiac('çinq')).to.equal('cinq');
    expect(rmDiac('Chapeaû')).to.equal('Chapeau');
    expect(rmDiac('Nørway')).to.equal('Norway');
    expect(rmDiac('There are ø solutions')).to.equal('There are o solutions');
    expect(rmDiac('Vålhalla')).to.equal('Valhalla');
    expect(rmDiac('JavascriptStraße')).to.equal('JavascriptStrase');
  });

  it('should return null on non-strings', () => {
    expect(rmDiac(42)).to.equal(null);
  });

});