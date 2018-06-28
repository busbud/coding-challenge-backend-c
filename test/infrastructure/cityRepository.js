const expect = require("chai").expect;
const es = require("event-stream");
const cityRepository = require("../../src/infrastructure/cityRepository")({
  dbFile: `${__dirname}/../fixture.tsv`
});

describe("CityRepository", () => {
  describe("findByName with partial name", () => {
    it("returns the matching cities from name field", done => {
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
        expect(result[0]).to.have.property("scoringName");

        done();
      });
    });

    it("returns the cities with scoringName and score", done => {
      cityRepository.findByName("Montr").then(result => {
        expect(result[0]).to.have.property("scoringName");
        expect(result[0]).to.have.property("score");

        done();
      });
    });

    it("returns the matching cities from ascii name field", done => {
      cityRepository.findByName("Montreal").then(result => {
        expect(result).to.be.an("array");

        expect(result[0]).to.have.property("name", "Montréal");
        expect(result[0]).to.have.property("nameAscii", "Montreal");

        done();
      });
    });

    it("returns cities with more than 5000 habitants", done => {
      cityRepository.findByName("Lorraine").then(result => {
        expect(result).to.be.empty;
        done();
      });
    });
  });

  describe("findByNameAndLocation with partial name", () => {
    it("returns the cities matching the name and located near the location", done => {
      cityRepository.findByNameAndLocation("Montr", { longitude: -73.58, latitude: 45.5 }).then(result => {
        expect(result).to.be.an("array");

        expect(result[0]).to.have.property("name", "Montréal");
        expect(result[0].location).to.have.property("longitude", -73.58781);
        expect(result[0].location).to.have.property("latitude", 45.50884);

        done();
      });
    });

    it("returns cities with more than 5000 habitants", done => {
      cityRepository.findByNameAndLocation("Lorraine", { longitude: -73.78249, latitude: 45.68338 }).then(result => {
        expect(result).to.be.empty;
        done();
      });
    });

    it("returns the cities with both scoringName, scoringDistance and score", done => {
      cityRepository.findByNameAndLocation("Montr", { longitude: -73.58, latitude: 45.5 }).then(result => {
        expect(result).to.be.an("array");
        expect(result[0]).to.have.property("scoringName");
        expect(result[0]).to.have.property("scoringDistance");
        expect(result[0]).to.have.property("score");

        done();
      });
    });

    it("returns no match if the location is too far", done => {
      cityRepository.findByNameAndLocation("Montreal", { longitude: -122.58, latitude: 45.5 }, 10).then(result => {
        expect(result).to.be.empty;

        done();
      });
    });
  });
});
