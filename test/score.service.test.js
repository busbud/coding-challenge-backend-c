const expect = require('chai').expect;
const ScoreService = require('../lib/score.service');

describe('ScoreService', () => {
  let scoreService;

  const montreal = { name: 'Montréal', lat: 45.50884, long: -73.58781 };

  before(() => {
    scoreService = new ScoreService();
  });

  describe('score function', () => {
    it('should return 1 for exact match', () => {
      montreal.distance = 0;
      expect(
        scoreService.score(montreal, {
          q: 'Montréal',
          latitude: 45.50884,
          longitude: -73.58781
        })
      ).to.equal(1);
    });

    it('should score using name and distance scorers', () => {
      montreal.distance = 10;
      expect(
        scoreService.score(montreal, {
          q: 'Mont',
          latitude: 42,
          longitude: -70
        })
      ).to.equal(0.5249999999999999);
    });

    it('should work if latitude and longitude are not specified', () => {
      expect(
        scoreService.score(montreal, {
          q: 'Montréal'
        })
      ).to.equal(1);
    });
  });
});
