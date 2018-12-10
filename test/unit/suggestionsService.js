var expect  = require('chai').expect;
var SuggestionsService     = require('../../code/SuggestionsService');
var suggestionsService = new SuggestionsService()

describe('getCitySuggestions()', function() {

  describe('without latitude and longitudes', function () {

      it('should return nothing with un unknown city name', function () {
          const suggestions = suggestionsService.getCitySuggestions('-----');
          expect(suggestions).to.be.instanceof(Array);
          expect(suggestions).to.have.length(0);
      });

      it('should return maximum of 50 suggestions', function () {
          const suggestions = suggestionsService.getCitySuggestions('Bromont');
          expect(suggestions).to.be.instanceof(Array);
          expect(suggestions).to.have.length(50);
      });

    });

});

describe('addNameScore()', function() {

  it('should add a score.name property', function () {
      const cities = [{
        name: 'toto'
      },{
        name: 'titi'
      },{
        name: 'tata'
      }];
      suggestionsService.addNameScore(cities, 'toto');
      expect(cities[0]).to.have.property('score');
      expect(cities[0].score).to.have.property('name')
  });

});


describe.only('addDistanceScore()', function() {

  it('should add a score.distance property', function () {
      const cities = [{
        lat: 1,
        long: 1
      },{
         lat: 0.5,
         long: -0.5
      },{
        lat: -0.5,
        long: 0.5
      }];
      suggestionsService.addDistanceScore(cities, 1, 1);
      expect(cities[0]).to.have.property('score');
      expect(cities[0].score).to.have.property('distance')
  });

  it('should set score 1 for a perfect match', function () {
      const cities = [{
        lat: 41,
        long: 91
      },{
         lat: 0.5,
         long: -0.5
      },{
        lat: -0.5,
        long: 0.5
      }];
      suggestionsService.addDistanceScore(cities, 41, 91);

      expect(cities[0].score.distance).to.equal(1);
  });

  it('should set score 0 when the lat and long is not in a correct format', function () {
      const cities = [{
        lat: 'tata',
        long: 'toto'
      },{
         lat: 0.5,
         long: -0.5
      },{
        lat: -0.5,
        long: 0.5
      }];
      suggestionsService.addDistanceScore(cities, 1, 1);
      expect(cities[0].score.distance).to.equal(0);
  });

});
