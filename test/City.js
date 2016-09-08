var rewire = require('rewire');
var expect = require('chai').expect;
var Query = require('../repos/Query');

var City = rewire('../repos/City');

var query = Query.make({q: "Montreal"});

describe('City Factory', () => {
  describe('constructor', () => {
    let matches = City.getByQuery(query);

    it('is not empty', () => {
      expect(matches.length).to.be.above(0);
    });

    it('contains a match', () => {
      expect(matches).to.satisfy(matches => {
        return matches.some(match => {
          return (new RegExp(query.search, 'i')).test(match.name+','+match.alt_names.join(','));
        });
      });
    });

    it('has the coordinates', () => {
      expect(matches).to.satisfy(matches => {
        return matches.every(match => {
          return match.latitude && match.longitude;
        });
      });
    });
  });

  describe('getStateSymbol', () => {
    var getStateSymbol = City.__get__('getStateSymbol');
    var city = {
      country: "",
      admin1: ""
    };

    it('returns admin1 with country being US', () => {
      city.country = "US";
      city.admin1 = "NY";

      let result = getStateSymbol(city);
      expect(result).to.equal(city.admin1);
    });

    it('returns mapped value with country being CA', () => {
      city.country = "CA";
      city.admin1 = "10";

      let result = getStateSymbol(city);
      expect(result).to.equal('QC');
    });
  });

  describe('getCountryName', () => {
    var getCountryName = City.__get__('getCountryName');
    var city = {
      country: ""
    };

    it('returns Canada for CA', () => {
      city.country = "CA";

      let result = getCountryName(city);
      expect(result).to.equal("Canada");
    });

    it('returns USA for US', () => {
      city.country = "US";

      let result = getCountryName(city);
      expect(result).to.equal("USA");
    });
  });
});
