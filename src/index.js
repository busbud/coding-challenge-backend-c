var fs = require('fs'),
    readline = require('readline'),
    Stream = require('stream'),
    util = require('util'),
    tsv = require('./tsv'),
    spatial = require('./spatial');

var Transform = Stream.Transform;

var rows = tsv.fromFile('data/cities_canada-usa.tsv');

var tree = new spatial.QuadTree()
rows
  .on('data', (row) => {
    tree.insert(row)
  })
  .on('end', () => {
    console.log(tree.retrieveClosest(44.38345, -64.51546))
  })
  .on('close', () => {
    console.log('closed')
  })
