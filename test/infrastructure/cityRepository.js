const expect = require("chai").expect;
const es = require("event-stream");
const cityRepository = require("../../src/infrastructure/cityRepository")({
  dbFile: `${__dirname}/../fixture.tsv`
});

describe("CityRepository", () => {
  describe("findByName with partial name", () => {
    it("return the matching cities from name field", done => {
      cityRepository.findByName("Montr").then(result => {
        expect(result).to.be.an("array");

        expect(result[0]).to.have.property("name");
        expect(result[0]).to.have.property("nameAscii");
        expect(result[0]).to.have.property("location");
        expect(result[0].location).to.have.property("longitude");
        expect(result[0].location).to.have.property("latitude");
        expect(result[0]).to.have.property("state");
        expect(result[0]).to.have.property("country");
        expect(result[0]).to.have.property("population");

        done();
      });
    });

    it("return the matching cities from ascii name field", done => {
      cityRepository.findByName("Montreal").then(result => {
        expect(result).to.be.an("array");

        expect(result[0]).to.have.property("name", "Montréal");
        expect(result[0]).to.have.property("nameAscii", "Montreal");

        done();
      });
    });

    it("return cities with more than 5000 habitants", done => {
      cityRepository.findByName("Lorraine").then(result => {
        expect(result).to.be.empty;
        done();
      });
    });
  });
});
