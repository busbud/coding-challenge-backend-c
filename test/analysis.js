
/********************
 * Import Modules
 *******************/

var expect = require("chai").expect;

var Analyzer = require(__dirname + "/../app/analysis/analyzer.js"),
  TokenizerBasic = require(__dirname + "/../app/analysis/tokenizer-basic.js"),
  TokenFilters = require(__dirname + "/../app/analysis/tokenfilters.js");





/********************
 * Tokenizer Basic Tests
 *******************/

describe("Tokenizer Basic", function () {
  it("must return the words of a space separated string", function () {
    var _string = "hello this is a test";
    var _tokens = TokenizerBasic(_string);
    //console.log(_tokens.join(","));
    expect(_tokens.length).to.equal(5);
  });
  it("must return the words containing ' and -", function () {
    var _string = "L'Aubaine-Folle";
    var _tokens = TokenizerBasic(_string);
    expect(_tokens.length).to.equal(1);
  });
  it("must take out the words from everything", function () {
    var _string = "somethin, (somethind) why?amen";
    var _tokens = TokenizerBasic(_string);
    expect(_tokens.length).to.equal(4);
  })
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
      var _strings1 = ["ha'ha L'Aubaine-Folle"],
        _strings2 = ["h-h L-Aubaine'Folle"];
      var _result1 = TokenFilters.ExpandSeparators(_strings1);
      var _result2 = TokenFilters.ExpandSeparators(_strings2);
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
      tokenizer: TokenizerBasic,
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
});