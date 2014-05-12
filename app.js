
/********************
 * Import Modules
 *******************/

var http = require("http");

var _ = require("lodash");

var Data = require("./app/data/data.js"),
  TSVImporter = require("./app/data/tsvimporter.js");

var Trie = require("./app/structures/trie.js");

var Analyzer = require("./app/analysis/analyzer.js"),
  TokenizerBasic = require("./app/analysis/tokenizer-basic.js"),
  TokenFilters = require("./app/analysis/tokenfilters.js");

var Ranking = require("./app/algorithms/ranking.js");




/********************
 * Setting Up Cache Trie Logic
 *******************/

var BusbudTrieNode = Trie.Node.extend({
  cacheSize: 10,

  rankWord: 1.0,
  rankCache: 0.7,

  objectComparison: function (a, b) {
    if (_.isNumber(a.population) && _.isNumber(b.population)) {
      return a.population - b.population;
    } else {
      return 0;
    }
  },
  computeRank: function (objects) {
    return objects;
  },
  formatData: function (object) {
    return _.cloneDeep(object);
  }
});

var BusbudTrie = Trie.extend({
  nodeClass: BusbudTrieNode
});


var trie = new Trie();




/********************
 * Setting Up Analyzer
 *******************/

var StandardAnalyzer = Analyzer.extend({
  tokenizer: TokenizerBasic,
  tokenFilters: [TokenFilters.LowerCase,
    TokenFilters.AsciiFolding, TokenFilters.ExpandSeparators]
});


var analyzer = new StandardAnalyzer();




/********************
 * Loading Data
 *******************/

var BusbudData = Data.extend({
  _idName: "id",
  validate: Data.BusbudGeonamesValidator
});

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
        console.log("100 Cities added!!");
      }
      // Element to be added to trie
      var _element = _elements[i];

      // Compute tokens that reverse index to object
      var _tokens = analyzer.analyze(_element.name);
        _tokens = _tokens.concat(analyzer.analyze(_element.name_alt));
      _tokens = _.uniq(_tokens);

      // Curiosity purposes
      if (i % 10 === 0) {
        console.log("City: " + i + " 10 Cities _tokens size: " + _tokens.length + ".");
        //console.log(_tokens);
      }

      // Add all tokens to trie, pointing to object
      for (var t = 0; t < _tokens.length; t++) {
        trie.addObject(_tokens[t], _element);
      }
    }
});




/********************
 * Node Server Set up
 *******************/

var port = process.env.PORT || 2345;

function SetupNode() {
  http.createServer(function (req, res) {
    res.writeHead(404, {'Content-Type': 'text/plain'});

    if (req.url.indexOf('/suggestions') === 0) {
      res.end(JSON.stringify({
        suggestions: []
      }));
    } else {
      res.end();
    }
  }).listen(port, '127.0.0.1');

  console.log('Server running at http://127.0.0.1:%d/suggestions', port);
}


