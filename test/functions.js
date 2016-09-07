var functions = require('../functions');
var expect = require('chai').expect;

describe('getCityNameMatch', function() {
  var query = {
    search: ""
  };
  var city = {
    name: "Montréal",
    alt_names: ["Montreal", "Монреаль"]
  }

    it('returns the exact match with the exact query', function() {
      query.search = "Montréal";

      let result = functions.getCityNameMatch(query, city);
      expect(result).to.equal(query.search);
    });

    it('returns the best match with the partial query', function() {
      query.search = "Mont";

      let result = functions.getCityNameMatch(query, city);
      expect(result).to.match(new RegExp(query.search));
    });

    it('returns the match in the language with the foreign language query', function() {
      query.search = "Монреаль";

      let result = functions.getCityNameMatch(query, city);
      expect(result).to.equal(query.search);
    })
});

describe('getStateSymbol', function() {
  var city = {
    country: "",
    admin1: ""
  };

  it('returns admin1 with country being US', function() {
    city.country = "US";
    city.admin1 = "NY";

    let result = functions.getStateSymbol(city);
    expect(result).to.equal(city.admin1);
  });

  it('returns mapped value with country being CA', function() {
    city.country = "CA";
    city.admin1 = "10";

    let result = functions.getStateSymbol(city);
    expect(result).to.equal('QC');
  });
});

describe('getCountryName', function() {
  var city = {
    country: ""
  };

  it('returns Canada for CA', function() {
    city.country = "CA";

    let result = functions.getCountryName(city);
    expect(result).to.equal("Canada");
  });

  it('returns USA for US', function() {
    city.country = "US";

    let result = functions.getCountryName(city);
    expect(result).to.equal("USA");
  });
});

describe('calculateScore', function() {
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

  it('returns max score for exact name match', function() {
    query.search = "Montreal";

    let result = functions.calculateScore(query, match);
    expect(result).to.equal(1);
  });

  it('returns score less than 1 for partial name match', function() {
    query.search = "Mont";

    let result = functions.calculateScore(query, match);
    expect(result).to.equal(0.5);
  });

  it('returns small score for query not matching beginning of the city name', function() {
    query.search = "real";

    let result = functions.calculateScore(query, match);
    expect(result).to.equal(0.5*0.5);
  });

  it('returns max score for short distance', function() {
    query.search = 'Mont';
    query.latitude = 45.4804391;
    query.longitude = -73.6255678;

    let result = functions.calculateScore(query, match);
    expect(result).to.equal(1);
  });

  it('returns score less than 1 for large distance', function() {
    query.search = 'Mont';
    query.latitude = 40.7055647;
    query.longitude = -74.1184291;

    let result = functions.calculateScore(query, match);
    expect(result).to.be.below(1);
  });
});
