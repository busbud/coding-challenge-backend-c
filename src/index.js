// var fs = require('fs'),
//     readline = require('readline'),
//     Stream = require('stream'),
//     util = require('util'),
//     spatial = require('./spatial');

var util = require('./util')
console.log(util.calculateScore(10))
console.log(util.calculateScore(25))
console.log(util.calculateScore(50))
console.log(util.calculateScore(100))
console.log(util.calculateScore(200))
console.log(util.calculateScore(1000))
console.log(util.calculateScore(5000))

// var fromFile = require('./tsv').fromFile;
// var fromStream = require('./spatial').fromStream;

// var rows = fromFile('data/cities_canada-usa.tsv');
// var res = fromStream(rows)
// var trie = res.trie;
// var tree = res.tree;

// rows.on('end', () => {
//   function recurse(leaf, length) {
//     if (leaf) {
//       return recurse(leaf.ne) +
//         recurse(leaf.nw) +
//         recurse(leaf.sw) +
//         recurse(leaf.se) +
//         1
//     } else {
//       return 0;
//     }
//   }
//   console.log(recurse(tree.root));
// })
