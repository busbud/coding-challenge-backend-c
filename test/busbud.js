
/********************
 * Import Modules
 *******************/

var expect = require("chai").expect;

var _ = require("lodash");


var Ranking = require(__dirname + "/../busbud/algorithms/ranking.js");

var Busbud = require(__dirname + "/../busbud/busbud.js");





/********************
 * Test Busbud Trie
 *******************/

describe("Busbud Trie", function () {
  var _busbud = null,
    _importingSuccess = true;

  before(function (done) {
    _busbud = new Busbud(__dirname + "/../data/cities_canada-usa.tsv", function (err, data) {
      // Check if error
      if (err) {
        _importingSuccess = false;
        done(err);
      }
      done(null);
    })
  });

  it("must have extended", function () {
    expect(_busbud.trie.nodeClass.prototype.rankCache).to.equal(0.2);
  });
  it("must not return an error for importing valid file", function () {
    expect(_importingSuccess).to.be.true;
  });
  it("must find somethin", function () {
    var _finds = _busbud.trie.traverse("new");
    //console.log("Found " + _finds.length + " results.");
    //console.log("First city:");
    //console.log(_finds[0]);
    //console.log("Last city:");
    //console.log(_finds[_finds.length - 1]);
    expect(_finds).to.be.instanceOf(Array);
    expect(_finds.length > 0).to.be.true;
  });
  it("must score and reduce results", function () {
    var _string = "new york",
      _outputTrie = [];

    var _tokens = _busbud.analyzer.analyze(_string);
    for (var t = 0; t < _tokens.length; t++) {
      _outputTrie.push(_busbud.trie.traverse(_tokens[t],  42.88645, -78.87837));
    }

    var _result = Ranking.ReduceRanking(_outputTrie, 10);
    //console.log(_result);
    expect(_result).to.be.instanceOf(Array);
  });
});

