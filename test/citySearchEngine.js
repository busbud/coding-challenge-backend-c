var citySearchEngine = require("../business/citySearchEngine");
var es = require("event-stream");
var expect = require("chai").expect;

describe.only("citySearchEngine tests:", function() {

    describe("when searching cities with a term, it", function() {

        it("must output json containing a filtered and ordered city list", function(done) {
            // init search engine
            citySearchEngine.init("data/cities_canada-usa-lite.tsv", function(engine) {

                var writer = es.writeArray(function (err, array) {
                    var actual = array.map(function(city){ return city.name; });
                    var expected = ['London, ON, CA', 'Longwood, FL, US', 'Longueuil, QC, CA', 'Longboat Key, FL, US'];

                    expect(actual).to.deep.equal(expected);
                    done();
                });

                // launch searching, scoring and sorting
                engine.searchStream({q:"lon"}).pipe(writer);
            });
            
        });

    });

});