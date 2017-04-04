var expect = require('chai').expect;
var cityIndex = require('../business/cityIndex');

describe("cityIndex tests:", function() {

    var index;

    beforeEach(function(){
        index = cityIndex.createIndex();
    });

    describe("when adding a city, it", function() {

        it("must throw an exception when no city is given", function() {
            var f = function() { index.addCity(); }
            expect(f).to.throw("Invalid city");
        });

        it("must throw an exception when the provided city is not an object", function() {
            var f = function() { index.addCity("New York"); }
            expect(f).to.throw("Invalid city");
        });

        it("must throw an exception since only objects with a 'name' property are accepted", function() {
            var f = function() { index.addCity({ alias: "New York"}); }
            expect(f).to.throw("Invalid city");
        });

        it("must store the city so that it can be found later", function() {
            index.addCity({ name: "AIX"});

            expect(index.level).to.equal(0);
            expect(index.subNodes).to.not.be.null;
            expect(index.subNodes["a"]).to.not.be.undefined;
            expect(index.subNodes["a"].level).to.equal(1);
            expect(index.subNodes["a"].cities.length).to.equal(1);
            
            expect(index.subNodes["a"].subNodes["i"]).to.not.be.undefined;
            expect(index.subNodes["a"].subNodes["i"].level).to.equal(2);
            expect(index.subNodes["a"].subNodes["i"].cities.length).to.equal(1);

            expect(index.subNodes["a"].subNodes["i"].subNodes["x"]).to.not.be.undefined;
            expect(index.subNodes["a"].subNodes["i"].subNodes["x"].level).to.equal(3);
            expect(index.subNodes["a"].subNodes["i"].subNodes["x"].cities.length).to.equal(1);
        });
        
        it("must store cities properly", function() {
            index.addCity({ name: "AIX"});
            index.addCity({ name: "AXE"});            
            index.addCity({ name: "AXI"});
            index.addCity({ name: "BAX"});

            expect(index.level).to.equal(0);
            expect(index.subNodes).to.not.be.null;

            var a = index.subNodes["a"];
            expect(a).to.not.be.undefined;
            expect(a.level).to.equal(1);
            expect(a.cities.length).to.equal(3);
            
            var ai = index.subNodes["a"].subNodes["i"];
            expect(ai).to.not.be.undefined;
            expect(ai.level).to.equal(2);
            expect(ai.cities.length).to.equal(1);
            
            var ax = index.subNodes["a"].subNodes["x"];
            expect(ax).to.not.be.undefined;
            expect(ax.level).to.equal(2);
            expect(ax.cities.length).to.equal(2);

            var aix = index.subNodes["a"].subNodes["i"].subNodes["x"];
            expect(aix).to.not.be.undefined;
            expect(aix.level).to.equal(3);
            expect(aix.cities.length).to.equal(1);
            
            var axe = index.subNodes["a"].subNodes["x"].subNodes["e"];
            expect(axe).to.not.be.undefined;
            expect(axe.level).to.equal(3);
            expect(axe.cities.length).to.equal(1);
        });
    });

    describe("when searching cities, it", function() {

        it("must find the 2 cities whose names starts with 'chic'", function(){
            index.addCity({ name: "Chicago"});
            index.addCity({ name: "Atlanta"});
            index.addCity({ name: "chicoutimi"});

            var cities = index.findCities("chic");

            expect(cities).to.not.be.null;
            expect(cities.length).to.equal(2);
        });
        
        it("must find the only city whose name starts with 'chico'", function(){
            index.addCity({ name: "Chicago"});
            index.addCity({ name: "Atlanta"});
            index.addCity({ name: "chicoutimi"});

            var cities = index.findCities("chico");

            expect(cities).to.deep.equal([{ name: "chicoutimi"}]);
        });
        
        it("must find the only city whose name starts with 'al'", function(){
            index.addCity({ name: "Chicago"});
            index.addCity({ name: "Atlanta"});
            index.addCity({ name: "chicoutimi"});
            index.addCity({ name: "AliFAx"});

            var cities = index.findCities("al");

            expect(cities).to.deep.equal([{ name: "AliFAx"}]);
        });
        
        it("must find cities having diacritics in their name", function(){
            index.addCity({ name: "Chicago"});
            index.addCity({ name: "Atlanta"});
            index.addCity({ name: "chicoutimi"});
            index.addCity({ name: "Montréal"});

            var cities = index.findCities("montreal");

            expect(cities).to.deep.equal([{ name: "Montréal"}]);
        });
        
        it("must not find anything when there are no matches", function(){
            index.addCity({ name: "Chicago"});
            index.addCity({ name: "Atlanta"});
            index.addCity({ name: "chicoutimi"});

            var cities = index.findCities("car");

            expect(cities).to.not.be.null;
            expect(cities.length).to.equal(0);
        });
        
    });
});