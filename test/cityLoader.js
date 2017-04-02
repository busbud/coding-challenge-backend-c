var cityIndex = require('../business/cityIndex');
var cityLoader = require("../business/cityLoader");
var expect = require('chai').expect;
var fs = require("fs");
var MemoryStream = require('memorystream');

describe("cityLoader tests :", function(){

    var memStream;

    describe("when loading cities, it", function() {

        beforeEach(function(){
            memStream = MemoryStream.createWriteStream();
        });

        it("must write json file containing cities", function(done) {
            var outfile = fs.createWriteStream(__dirname + "/cities.json");
            var readStream = cityLoader.loadAndWriteTo(outfile);
            readStream.on("finish", done);
        });

        it("should load expected data", function(done) {
            var readStream = cityLoader.loadAndWriteTo(memStream);

            readStream.on("finish", function() {
                var cities = JSON.parse(memStream.toString());

                expect(cities).to.be.not.null;
                expect(cities.suggestions).to.be.not.undefined;
                expect(cities.suggestions).to.be.not.null;
                expect(cities.suggestions.length).to.equal(7237);
                
                done();
            });
        });
    });

    describe("when loading 1000 cities, it", function() {

        it("must store them in the index", function(done) {
            var index = cityIndex.createIndex();
            var readStream = cityLoader.loadAndStoreTo(index, "data/cities_canada-usa-lite.tsv");

            readStream.on("end", function() {
                // var expectedCities = [
                //     "Sooke, BC, CA",
                //     "Sorel-Tracy, QC, CA",
                //     "South Huron, ON, CA",
                //     "Southside, AL, US",
                //     "South Bradenton, FL, US",
                //     "South Apopka, FL, US",
                //     "South Daytona, FL, US",
                //     "South Gate Ridge, FL, US",
                //     "South Highpoint, FL, US",
                //     "South Miami, FL, US",
                //     "South Miami Heights, FL, US",
                //     "South Patrick Shores, FL, US",
                //     "South Venice, FL, US",
                //     "Southchase, FL, US",
                //     "Southgate, FL, US",
                //     "Southwest Ranches, FL, US"
                // ];
                var expectedCities = [
                    "Sooke",
                    "Sorel-Tracy",
                    "South Huron",
                    "Southside",
                    "South Bradenton",
                    "South Apopka",
                    "South Daytona",
                    "South Gate Ridge",
                    "South Highpoint",
                    "South Miami",
                    "South Miami Heights",
                    "South Patrick Shores",
                    "South Venice",
                    "Southchase",
                    "Southgate",
                    "Southwest Ranches"
                ];

                var cities = index.findCities("So");

                expect(cities).to.not.be.null;
                expect(cities.length, "There should be 16 cities starting with 'So'").to.equal(16);
                expect(cities.every(function(city){ return expectedCities.indexOf(city.name) > -1; }), "Some expected cities have not been found in the search results").to.be.true;
                done();
            });
        });
    });
    
    describe("when loading all cities, it", function() {

        it("must store them in the index", function(done) {
            var index = cityIndex.createIndex();
            var readStream = cityLoader.loadAndStoreTo(index, "data/cities_canada-usa.tsv");

            readStream.on("end", function() {                
                var cities = index.findCities("So");

                expect(cities).to.not.be.null;
                expect(cities.length, "There should be 119 cities starting with 'So'").to.equal(119);
                done();
            });
        });
        
    });
});