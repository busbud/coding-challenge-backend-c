//Tests scoring
var expect  = require('chai').expect;
var score = require('../score');

describe('Score suggestion', function() {
	describe('Normalized score on coords', function() {
		montreal_coord= {latitude: 45.50884, longitude: -73.58781 };
		sydney_coord= {latitude: -33.5, longitude: 151.1};
		it('is 1 at no distance', function() {
			expect(score._normScoreOnCoord(montreal_coord,montreal_coord)).to.equal(1);
		});
		it('is 0 at long distances', function() {
			expect(score._normScoreOnCoord(montreal_coord,sydney_coord)).to.equal(0);
		});
	});

	describe('Normalized score on population', function() {
		it('is 1 for large population', function() {
			expect(score._normScoreOnPopulation(20000000)).to.equal(1);
		});
		it('is 0 for no population', function() {
			expect(score._normScoreOnPopulation(0)).to.equal(0);
		});
	});

	//TODO: test on real city data; test final scores
});