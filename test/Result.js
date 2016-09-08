var rewire = require("rewire");
var expect = require('chai').expect;
var Query = require('../repos/Query');
var City = require('../repos/City');

var Result = rewire('../repos/Result');

describe('Result Factory', () => {
  describe('constructor', () => {
    let query = Query.make({q: "Montreal"});
    var results = Result.convertFromMatches(query, City.getByQuery(query));

    it('has formatted city names', () => {
      expect(results).to.satisfy(results => {
        return results.every(result => {
          return /[A-Za-z -]*, [A-Z]{2,}, (Canada|USA)/.test(result.name);
        });
      });
    });

    it('has latitude and longitude', () => {
      expect(results).to.satisfy(results => {
        return results.every(result => {
          return result.latitude && result.longitude;
        });
      });
    });

    it('has the score', () => {
      expect(results).to.satisfy(results => {
        return results.every(result => {
          return result.score;
        });
      });
    });
  });

  describe('calculateScore', () => {
    var calculateScore = Result.__get__('calculateScore');
    var query = {
      search: "",
      latitude: "",
      longitude: ""
    };
    var match = {
      name: "Montréal",
      alt_names: ["Montreal", "Монреаль"],
      latitude: 45.5016889,
      longitude: -73.567256
    };

    it('returns max score for exact name match', () => {
      query.search = "Montreal";

      let result = calculateScore(query, match);
      expect(result).to.equal(1);
    });

    it('returns score less than 1 for partial name match', () => {
      query.search = "Mont";

      let result = calculateScore(query, match);
      expect(result).to.equal(0.5);
    });

    it('returns small score for query not matching beginning of the city name', () => {
      query.search = "real";

      let result = calculateScore(query, match);
      expect(result).to.equal(0.5*0.5);
    });

    it('returns max score for short distance', () => {
      query.search = 'Mont';
      query.latitude = 45.4804391;
      query.longitude = -73.6255678;

      let result = calculateScore(query, match);
      expect(result).to.equal(1);
    });

    it('returns score less than 1 for large distance', () => {
      query.search = 'Mont';
      query.latitude = 40.7055647;
      query.longitude = -74.1184291;

      let result = calculateScore(query, match);
      expect(result).to.be.below(1);
    });
  });

  describe('getCityNameMatch', () => {
    var getCityNameMatch = Result.__get__('getCityNameMatch');
    var query = {
      search: ""
    };
    var city = {
      name: "Montréal",
      alt_names: ["Montreal", "Монреаль"]
    };

    it('returns the exact match with the exact query', () => {
      query.search = "Montréal";

      let result = getCityNameMatch(query, city);
      expect(result).to.equal(query.search);
    });

    it('returns the best match with the partial query', () => {
      query.search = "Mont";

      let result = getCityNameMatch(query, city);
      expect(result).to.match(new RegExp(query.search));
    });

    it('returns the match in the language with the foreign language query', () => {
      query.search = "Монреаль";

      let result = getCityNameMatch(query, city);
      expect(result).to.equal(query.search);
    });
  });
});
