
/********************
 * Import Modules
 *******************/

var expect = require("chai").expect;

var _ = require("lodash");

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
    expect(_busbud.trie.nodeClass.prototype.rankCache).to.equal(0.7);
  });
  it("must not return an error for importing valid file", function () {
    expect(_importingSuccess).to.be.true;
  });
  it("must find somethin", function () {
    var _finds = _busbud.trie.traverse("new");
    console.log("Found " + _finds.length + " results.");
    console.log("First city:");
    console.log(_finds[0]);
    expect(_finds).to.be.instanceOf(Array);
    expect(_finds.length > 0).to.be.true;
  });
});

