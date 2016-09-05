var functions = require('../functions');
var expect = require('chai').expect;

describe('getCityNameMatch', function() {
  var primaryName = "Montréal"
    , altNames = "Montreal,Монреаль";

    it('returns the exact match with the exact query', function() {
      var query = "Montréal";

      let result = functions.getCityNameMatch(query, primaryName, altNames);
      expect(result).to.equal(query);
    });

    it('returns the best match with the partial query', function() {
      var query = "Mont";

      let result = functions.getCityNameMatch(query, primaryName, altNames);
      expect(result).to.match(new RegExp(query));
    });

    it('returns the match in the language with the foreign language query', function() {
      var query = "Монреаль";

      let result = functions.getCityNameMatch(query, primaryName, altNames);
      expect(result).to.equal(query);
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
  var countryCode = "";

  it('returns Canada for CA', function() {
    countryCode = "CA";

    let result = functions.getCountryName(countryCode);
    expect(result).to.equal("Canada");
  });

  it('returns USA for US', function() {
    countryCode = "US";

    let result = functions.getCountryName(countryCode);
    expect(result).to.equal("USA");
  });
});

describe('calculateScore', function() {
  var query = ""
    , latitude = 0
    , longitude = 0
    , match = {
      name: "Montréal",
      alt_name: "Montreal,Монреаль",
      lat: 45.5016889,
      long: -73.567256
    };

  it('returns max score for exact name match', function() {
    query = "Montreal";

    let result = functions.calculateScore(query, latitude, longitude, match);
    expect(result).to.equal(1);
  });

  it('returns score less than 1 for partial name match', function() {
    query = "Mont";

    let result = functions.calculateScore(query, latitude, longitude, match);
    expect(result).to.equal(0.5);
  });

  it('returns small score for query not matching beginning of the city name', function() {
    query = "real";

    let result = functions.calculateScore(query, latitude, longitude, match);
    expect(result).to.equal(0.5*0.5);
  });

  it('returns max score for short distance', function() {
    query = 'Mont';
    latitude = 45.4804391;
    longitude = -73.6255678;

    let result = functions.calculateScore(query, latitude, longitude, match);
    expect(result).to.equal(1);
  });

  it('returns score less than 1 for large distance', function() {
    query = 'Mont';
    latitude = 40.7055647;
    longitude = -74.1184291;

    let result = functions.calculateScore(query, latitude, longitude, match);
    expect(result).to.be.below(1);
  });
});
