var es = require("event-stream");
var expect = require("chai").expect;
var scoreCalculator = require("../business/scoreCalculator");

describe("scoreCalculator tests : ", function() {

    describe("when providing a search term only, it", function() {

        it("must compute a score (for each city) based on the city name only", function(done) {
            var cities = [
                {"name":"London","latitude":"42.98339","longitude":"-81.23304", expectedScore: 0.8},
                {"name":"Longueuil","latitude":"45.53121","longitude":"-73.51806", expectedScore: 0.7},
                {"name":"Longboat Key","latitude":"27.41254","longitude":"-82.65899", expectedScore: 0.6},
                {"name":"Longwood","latitude":"28.70305","longitude":"-81.3384", expectedScore: 0.8},
                {"name":"London","latitude":"37.12898","longitude":"-84.08326", expectedScore: 0.8},
                {"name":"Londontowne","latitude":"38.93345","longitude":"-76.54941", expectedScore: 0.6},
                {"name":"Long Beach","latitude":"30.35048","longitude":"-89.15282", expectedScore: 0.7},
                {"name":"Long Beach","latitude":"33.91045","longitude":"-78.11777", expectedScore: 0.7},
                {"name":"London","latitude":"39.88645","longitude":"-83.44825", expectedScore: 0.8},
                {"name":"Lone Grove","latitude":"34.17537","longitude":"-97.26279", expectedScore: 0.7},
                {"name":"Longview","latitude":"32.5007","longitude":"-94.74049", expectedScore: 0.8},
                {"name":"Long Grove","latitude":"42.17836","longitude":"-87.99785", expectedScore: 0.7},
                {"name":"Longmeadow","latitude":"42.0501","longitude":"-72.58287", expectedScore: 0.7},
                {"name":"Londonderry","latitude":"42.86509","longitude":"-71.37395", expectedScore: 0.6}
            ];

            var writer = es.writeArray(function (err, array) {
                array.forEach(function(city) {
                    expect(city.score).to.equal(''+city.expectedScore);
                });
                done();
            });

            scoreCalculator.computeScores(cities, "lon", null).pipe(writer);
        });
    });
    
    describe("when providing a search term and a location, it", function() {

        it("must compute a score (for each city) based on the city name and the distance from the provided location", function(done) {
            var cities = [
                {"name":"London","latitude":"42.98339","longitude":"-81.23304", expectedScore: 0.1},
                {"name":"Longueuil","latitude":"45.53121","longitude":"-73.51806", expectedScore: 0.3},
                {"name":"Longboat Key","latitude":"27.41254","longitude":"-82.65899", expectedScore: 0.1},
                {"name":"Longwood","latitude":"28.70305","longitude":"-81.3384", expectedScore: 0.1},
                {"name":"London","latitude":"37.12898","longitude":"-84.08326", expectedScore: 0.1},
                {"name":"Londontowne","latitude":"38.93345","longitude":"-76.54941", expectedScore: 0.1},
                {"name":"Long Beach","latitude":"30.35048","longitude":"-89.15282", expectedScore: 0.1},
                {"name":"Long Beach","latitude":"33.91045","longitude":"-78.11777", expectedScore: 0.1},
                {"name":"London","latitude":"39.88645","longitude":"-83.44825", expectedScore: 0.1},
                {"name":"Lone Grove","latitude":"34.17537","longitude":"-97.26279", expectedScore: 0.1},
                {"name":"Longview","latitude":"32.5007","longitude":"-94.74049", expectedScore: 0.1},
                {"name":"Long Grove","latitude":"42.17836","longitude":"-87.99785", expectedScore: 0.1},
                {"name":"Longmeadow","latitude":"42.0501","longitude":"-72.58287", expectedScore: 0.6},
                {"name":"Londonderry","latitude":"42.86509","longitude":"-71.37395", expectedScore: 0.8}
            ];

            var writer = es.writeArray(function (err, array) {
                array.forEach(function(city) {
                    expect(city.score).to.equal(''+city.expectedScore);
                });
                done();
            });

            scoreCalculator.computeScores(cities, "lon", {"latitude": 42.86509, "longitude": -71.37395 }).pipe(writer);
        });
    });
});