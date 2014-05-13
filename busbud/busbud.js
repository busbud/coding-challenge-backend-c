
/********************
 * Import Modules
 *******************/

var _ = require("lodash");

var Data = require("./data/data.js"),
  TSVImporter = require("./data/tsvimporter.js");

var Trie = require("./structures/trie.js");

var Analyzer = require("./analysis/analyzer.js"),
  TokenizerBasic = require("./analysis/tokenizer-basic.js"),
  TokenFilters = require("./analysis/tokenfilters.js");

var Ranking = require("./algorithms/ranking.js");




/********************
 * Setting Up Cache Trie Logic
 *******************/

var BusbudTrieNode = Trie.Node.extend({
  cacheSize: 10,

  rankWord: 1.0,
  rankCache: 0.2,

  objectComparison: function (a, b) {
    if (_.isNumber(a.population) && _.isNumber(b.population)) {
      return a.population - b.population;
    } else {
      return 0;
    }
  },
  computeRank: Ranking.WordCacheGeoRanking,
  formatData: function (object) {
    //console.log("Formating Data.");
    // Result to be returned
    var _result = {};

    // String for name
    if (object.province) {
      _result.name = object.name + ", " + object.province + ", " + object.country;
    } else {
      _result.name = object.name + ", " + object.country;
    }

    // Other info
    _result.rank = _.cloneDeep(object.rank); // dont want references
    _result.geo = {};
    _result.geo.lat = object.geo.lat;
    _result.geo.lon = object.geo.lon;

    return _result;
  }
});

var BusbudTrie = Trie.extend({
  nodeClass: BusbudTrieNode
});





/********************
 * Setting Up Analyzer
 *******************/

var NameAnalyzer = Analyzer.extend({
  tokenizer: TokenizerBasic.EuropeanName,
  tokenFilters: [TokenFilters.LowerCase, TokenFilters.ExpandSeparators]
});


var analyzer = new NameAnalyzer();




/********************
 * Loading Data
 *******************/

var BusbudData = Data.extend({
  _idName: "id",
  validate: Data.BusbudGeonamesValidator
});

var BusbudTSVImporter = TSVImporter.extend({
  analyzer: analyzer,
  dataClass: BusbudData,
  processData:  function (data) {
    // Attach Data and Trie on this object
    this.data = data;
    this.trie = new BusbudTrie();

    // Compute basic ranking
    Ranking.BasicRanking(data.getDataList());

    // Import data into trie
    var _elements = data.getDataList();
    for (var i = 0; i < _elements.length; i++) {
      // For curiosity purposes
      //if (i % 1000 === 0) {
      //  console.log("1000 Cities added!!");
      //}

      // Element to be added to trie
      var _element = _elements[i];

      // Compute tokens that reverse index to object
      var _tokens = analyzer.analyze(_element.name);
        _tokens = _tokens.concat(analyzer.analyze(_element.name_alt));

      // Add all tokens to trie, pointing to object
      for (var t = 0; t < _tokens.length; t++) {
        this.trie.addObject(_tokens[t], _element);
      }
    }
  }
});


/*
var tsvBusbudDataImporter = new TSVImporter(__dirname + "/data/cities_canada-usa.tsv",
  BusbudData, function (err, data) {
    // Check if error
    if (err) {
      // !!! This exception is thrown during initialization and not during runtime !!!
      throw new Error(err);
    }

    // Compute basic ranking
    Ranking.BasicRanking(data.getDataList());

    // Import data into trie
    var _tokenSizeMax = 0;
    var _elements = data.getDataList();
    for (var i = 0; i < _elements.length; i++) {
      // For curiosity purposes
      if (i % 100 === 0) {
        console.log("200 Cities added!!");
      }
      // Element to be added to trie
      var _element = _elements[i];

      // Compute tokens that reverse index to object
      var _tokens = analyzer.analyze(_element.name);
        _tokens = _tokens.concat(analyzer.analyze(_element.name_alt));

      // Add all tokens to trie, pointing to object
      for (var t = 0; t < _tokens.length; t++) {
        trie.addObject(_tokens[t], _element);
      }
    }
});*/




/********************
 * Export Modules
 *******************/

module.exports = BusbudTSVImporter;