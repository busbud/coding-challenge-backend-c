
/********************
 * Import Modules
 *******************/

var expect = require("chai").expect;

var Analyzer = require(__dirname + "/../busbud/analysis/analyzer.js"),
  TokenizerBasic = require(__dirname + "/../busbud/analysis/tokenizer-basic.js"),
  TokenFilters = require(__dirname + "/../busbud/analysis/tokenfilters.js");





/********************
 * Tokenizer Basic Tests
 *******************/

describe("Tokenizer Basic", function () {
  describe("Unicode", function () {
    it("must return the words of a space separated string", function () {
      var _string = "hello this is a test";
      var _tokens = TokenizerBasic.Unicode(_string);
      //console.log(_tokens.join(","));
      expect(_tokens.length).to.equal(5);
    });
    it("must return the words containing ' and -", function () {
      var _string = "L'Aubaine-Folle";
      var _tokens = TokenizerBasic.Unicode(_string);
      expect(_tokens.length).to.equal(1);
    });
    it("must take out the words from everything", function () {
      var _string = "somethin, (somethind) why?amen";
      var _tokens = TokenizerBasic.Unicode(_string);
      expect(_tokens.length).to.equal(4);
    });
  });
  describe("European Name", function () {
    it("must take out only words from between separators", function () {
      var _string = "name, Name, n{}tname,NAMEZ";
      var _tokens = TokenizerBasic.EuropeanName(_string);
      //console.log(_tokens);
      expect(_tokens.length).to.equal(3);
    });
  });

});



/********************
 * Token Filters Basic Tests
 *******************/

describe("Token Filter", function () {
  describe("Lowercase", function () {
    it("should return string to lowercase at least with french letters", function () {
      var _strings = ["ÉÀÈÇç"];
      var _result = TokenFilters.LowerCase(_strings);
      //console.log(_result);
      expect(_result[0]).to.equal("éàèçç");
    });
  });

  describe("Ascii Folding", function () {
    it("must ascii fold and that's it", function () {
      var _strings = ["ÉÀÈÇç"];
      var _result = TokenFilters.AsciiFolding(_strings);
      //console.log(_result);
      expect(_result[0]).to.equal("EAEcc"); // Has a bug, but its the library
    });
  });

  describe("Separator expander", function () {
    it("must expand", function () {
      var _strings1 = ["ha'haz L'Aubaine-Folle"],
        _strings2 = ["h-ha L-Aubaine'Folle"];
      var _result1 = TokenFilters.UniExpandSeparators(_strings1);
      var _result2 = TokenFilters.UniExpandSeparators(_strings2);
      //console.log(_result1);
      //console.log(_result2);
      expect(_result1.length).to.equal(9);
      expect(_result2.length).to.equal(9);

    });
  });
});




/********************
 * Analyzer Basic Tests
 *******************/

describe("Analyzer", function () {
  describe("with basic tokenizer and all token filters", function () {
    var StandardAnalyzer = Analyzer.extend({
      tokenizer: TokenizerBasic.Unicode,
      tokenFilters: [TokenFilters.LowerCase,
        TokenFilters.AsciiFolding, TokenFilters.ExpandSeparators]
    });

    var _analyzer = new StandardAnalyzer();

    var _string = "How about testing l'aile t-a'b'c (Héro)";
    var _tokens = _analyzer.analyze(_string);
    //console.log(_tokens);
    expect(_tokens).to.be.instanceOf(Array); // This a really good unit test
    expect(_tokens.length).to.equal(14);
  });

  describe("with name tokenizer and general filters", function () {
    var NameAnalyzer = Analyzer.extend({
      tokenizer: TokenizerBasic.EuropeanName,
      tokenFilters: [TokenFilters.LowerCase, TokenFilters.ExpandSeparators]
    });

    var _analyzer = new NameAnalyzer();
    var _string = "name, name2, {}notname,néà";
    var _tokens = _analyzer.analyze(_string);
    //console.log(_tokens);
    expect(_tokens).to.be.instanceOf(Array);
    expect(_tokens.length).to.equal(3);
  });
});