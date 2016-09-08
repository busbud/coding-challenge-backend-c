var expect = require('chai').expect;
var Query = require('../repos/Query');

describe('Query Factory', () => {
  describe('constructor', () => {
    it('should return 0 for coordinates', () => {
      let originalQuery = {
        q: "Montreal"
      };
      let parsedQuery = Query.make(originalQuery);

      expect(parsedQuery).to.eql({
        search: originalQuery.q,
        latitude: 0,
        longitude: 0
      });
    });

    it('should add coordinates to the object', () => {
      let originalQuery = {
        q: "Montreal",
        latitude: "45.4804391",
        longitude: "-73.6255678"
      };
      let parsedQuery = Query.make(originalQuery);

      expect(typeof parsedQuery.latitude).to.equal("number");
      expect(typeof parsedQuery.longitude).to.equal("number");
      expect(parsedQuery).to.eql({
        search: originalQuery.q,
        latitude: 45.4804391,
        longitude: -73.6255678
      });
    });
  });
});
