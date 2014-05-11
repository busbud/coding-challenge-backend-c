
/********************
 * Import Modules
 *******************/

var expect = require("chai").expect;

var _ = require("lodash");


var Data = require(__dirname + "/../app/data/data.js"),
  TSVImporter = require(__dirname + "/../app/data/tsvimporter.js");

var Ranking = require(__dirname + "/../app/algorithms/ranking.js");




/********************
 * Test JavaScript
 *******************/

// Yeah ... wanted to make sure it works like I thought
describe("Javascript", function () {
  it("must be able to transfer by reference between arrays", function () {
    var _a = [],
      _b = [],
      _d = { e: "f", g: "h" };

    _a.push(_d);
    _b.push(_d);
    _a[0].e = "z";
    expect(_b[0].e).to.be.equal("z");
  });
});




/********************
 * Test Ranking
 *******************/

describe("Algorithm", function () {
  describe("Statistics of cities", function () {
    // Variables
    var _tsvCities = null,
      _data = null;

    var GeonamesData = Data.extend({
      validate: Data.BusbudGeonamesValidator
    });

    before(function (done) {
      _tsvCities = new TSVImporter(__dirname + "/../data/cities_canada-usa.tsv", GeonamesData, function (err, data) {
        // Check error
        if (err) {
          return done(err);
        }

        _data = data;
        return done(null);
      });
    });

    it("should have statistics", function() {
      var _rankingBasic = Ranking.BasicRanking(_data.getDataList());
      expect(_rankingBasic).to.be.instanceOf(Object);
      console.log("Statistics: ");
      console.log(_rankingBasic.statistics);
      console.log("Rank of biggest city: ");
      console.log(_rankingBasic.dataListOrdSize[0]);
      console.log("Rank of smallest city: ");
      console.log(_rankingBasic.dataListOrdSize[_rankingBasic.dataListOrdSize.length-1]);
    });
  });
});