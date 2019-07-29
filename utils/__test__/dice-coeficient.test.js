const dice = require('../dice-coeficient');

describe('diceCoeficient', () => {
  test('should return 1', () => {
    expect(dice('londo', 'london')).toEqual(0.8888888888888888)
  });
});