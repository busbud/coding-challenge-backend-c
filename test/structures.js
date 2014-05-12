
/********************
 * Import Modules
 *******************/

var expect = require("chai").expect;

var Classy = require(__dirname + "/../busbud/structures/classy.js"),
  Trie = require(__dirname + "/../busbud/structures/trie.js");





/********************
 * Test Classy
 *******************/

describe("Classy class", function () {
  // Classes to test on
  var Parent = Classy.extend({
    constructor: function () {},
    getID: function () {
      return 1;
    }
  });
  var Child = Parent.extend({
    constructor: function () {},
    getID: function () {
      return 2;
    },
    getParentID: function () {
      return Child.__super__.getID();
    }
  });
  var GrandChild = Child.extend({
    constructor: function () {},
    getID: function () {
      return 3;
    }
  });

  describe("Child instance", function () {
    var _parent = new Parent(),
      _child = new Child();

    it("should have the constructor Child", function () {
      expect(_child.constructor).to.equal(Child);
    });
    it("should be an instance of class Child and Parent that he inherited, but not GrandChild", function () {
      expect(_child).to.be.instanceOf(Parent);
      expect(_child).to.be.instanceOf(Child);
      expect(_child).not.to.be.instanceOf(GrandChild);
    });
    it("should have a parent that is Parent", function () {
      expect(Child.__super__).to.equal(Parent.prototype);
    });
    it("should have id 2", function () {
      expect(_child.getID()).to.equal(2);
    });
    it("should have a parent with id 1", function () {
      expect(_child.getParentID()).to.equal(1);
      expect(_child._super_.getID()).to.equal(1);
    });
  });

  describe("GrandChild instance", function () {
    var _parent = new Parent(),
      _child = new Child(),
      _grand = new GrandChild();

    it("should have id 3", function () {
      expect(_grand.getID()).to.equal(3);
    });
    it("should have a parent that is Child", function () {
      expect(GrandChild.__super__).to.equal(Child.prototype);
    });
    it("should have parent with id 2", function () {
      expect(_grand._super_.getID()).to.equal(2);
    });
  })
});





/********************
 * Test Trie
 *******************/

describe("Trie Class", function () {
  // Data for trie
  var _strings = ["allo", "this", "hello", "hi", "hemmington", "heroku"],
    _objects = [{ rankError: 1.0 }, { ranky: 0.1 }, { ranky: 0.2 },
      { ranky: 0.3 }, { ranky: 0.5 }, { ranky: 10.0 }];

  // Extend for test
  var ENode = Trie.Node.extend({
    cacheSize: 5,
    rankWord: 0.75,
    rankCache: 0.6,
    objectComparison: function (a, b) {
      return this.ranky(a) - this.ranky(b);
    },
    ranky: function (obj) {
      if (obj.ranky) {
        return obj.ranky;
      } else {
        return 0.0;
      }
    }
  });
  var ETrie = Trie.extend({
    nodeClass: ENode
  });

  var _trie = new ETrie();

  it("must be able to populate", function () {
    for (var i = 0; i < _strings.length; i++) {
      var _result = _trie.addObject(_strings[i], _objects[i]);
      expect(_result).to.be.true;
    }
  });
  it("must be able to get the data back, in this case cache", function () {
    var _result = _trie.traverse("h");
    expect(_result).to.be.instanceOf(Array);
    expect(_result.length).to.equal(4)
  });
  it("must be able to get a specific data back", function () {
    var _result = _trie.traverse("hello");
    expect(_result).to.be.instanceOf(Array);
    expect(_result.length).to.equal(1);
    expect(_result[0].ranky).to.equal(0.2);
  });

});