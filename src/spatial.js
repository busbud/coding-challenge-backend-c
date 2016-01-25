"use strict";
var Stream = require('stream');
var Transform = Stream.Transform;
var util = require('./util');
var Triejs = require('triejs');

// Calculates the distance between two lat/long pairs
// in KM.
// https://en.wikipedia.org/wiki/Haversine_formula
// http://stackoverflow.com/a/27943
var R = 6371; // Mean radius of the earth in km
function distance(leaf1, leaf2) {
  var degreeLat = degreeToRad(leaf2.latitude-leaf1.latitude);
  var degreeLong = degreeToRad(leaf2.longitude-leaf1.longitude);
  var a =
        Math.sin(degreeLat/2) * Math.sin(degreeLat/2) +
        Math.cos(degreeToRad(leaf1.latitude)) * Math.cos(degreeToRad(leaf2.latitude)) *
        Math.sin(degreeLong/2) * Math.sin(degreeLong/2);
  var c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1-a));
  var d = R * c; // Distance in km
  return d;
}

// Converts degrees to radians
function degreeToRad(degrees) {
  return degrees * (Math.PI / 180);
}

// This class can be used as a stream.Transform and
// as a standalone QuadTree implementation by calling
// the `insert` method.
class QuadTree extends Transform {
  constructor() {
    super({objectMode: true});
  }

  _transform(row, enc, done) {
    this.insert(row, undefined);
    this.push(row);
    done();
  }

  insert(newLeaf, parentLeaf) {
    // The root hasn't been set yet.
    if (!this.root) {
      // Javascript object used instead of arrays
      // for readability.
      // this.root = {
      //   nw: undefined, // North-West quadrant
      //   ne: undefined, // North-East quadrant
      //   se: undefined,
      //   sw: undefined,
      //   latitude: latitude,
      //   longitude: longitude
      // }

      this.root = newLeaf;
      return this.root;

    } else if (parentLeaf) {
      // NE
      if (newLeaf.latitude >= parentLeaf.latitude &&
          newLeaf.longitude >= parentLeaf.longitude) {
        if (!parentLeaf.ne)
          // If no more tree traversal is required
          parentLeaf.ne = newLeaf
        else
          return this.insert(newLeaf, parentLeaf.ne)
      }

      // NW
      else if (newLeaf.latitude >= parentLeaf.latitude &&
               newLeaf.longitude <= parentLeaf.longitude) {
        if (!parentLeaf.nw)
          parentLeaf.nw = newLeaf
        else
          return this.insert(newLeaf, parentLeaf.nw)
      }

      // SW
      else if (newLeaf.latitude <= parentLeaf.latitude &&
               newLeaf.longitude <= parentLeaf.longitude) {
        if (!parentLeaf.sw)
          parentLeaf.sw = newLeaf
        else
          return this.insert(newLeaf, parentLeaf.sw)
      }

      // SE
      else if (newLeaf.latitude <= parentLeaf.latitude &&
               newLeaf.longitude >= parentLeaf.longitude) {
        if (!parentLeaf.se)
          parentLeaf.se = newLeaf
        else
          return this.insert(newLeaf, parentLeaf.se)
      }

      newLeaf.parent = parentLeaf
      return newLeaf

    } else {
      return this.insert(newLeaf, this.root);
    }
  }

  getNearby(leaf, maxDistance, maxNeighbours) {
    // An object is used instead of a set in order
    // to keep track of neighbours because `getNearby`
    // clones the neighbour.
    const neighbours = {};

    function climb(parent) {
      if (Object.keys(neighbours).length >= maxNeighbours)
        return;
      if (!parent)
        return;

      let dist = distance(leaf, parent)
      if (dist <= maxDistance) {
        let clonedParent = util.clone(parent);
        clonedParent.distance = dist;
        neighbours[clonedParent.geonameid] = clonedParent;
      }

      descend(parent.ne);
      descend(parent.nw);
      descend(parent.se);
      descend(parent.sw);
      climb(parent.parent);
    }

    function descend(child) {
      if (!child)
        return;
      if (Object.keys(neighbours).length >= maxNeighbours)
        return;
      if (child.latitude == leaf.latitude &&
          child.longitude == leaf.longitude)
        return;

      let dist = distance(leaf, child)
      if (dist <= maxDistance) {
        let clonedChild = util.clone(child);
        clonedChild.distance = dist;
        neighbours[clonedChild.geonameid] = clonedChild
      }

      descend(child.ne);
      descend(child.nw);
      descend(child.se);
      descend(child.sw);
    }

    climb(leaf.parent);
    descend(leaf.ne);
    descend(leaf.nw);
    descend(leaf.se);
    descend(leaf.sw);

    // Remove duplicates
    const neighboursArr = [];
    for (var k in neighbours) {
      neighboursArr.push(neighbours[k]);
    }
    return neighboursArr;
  }
}

function fromStream(stream) {
  const tree = new QuadTree();
  const trie = new Triejs();
  stream.on('data', (row) => {
    if (row.countryCode !== 'US' && row.countryCode !== 'CA') {
      return;
    }

    // Add all cities to tree.
    let leaf = tree.insert(row);

    // Don't add to the trie if pop less than 5000
    if(row.population < 5000) {
      return;
    }

    let name = row.name;
    trie.add(name, leaf);
    if (name !== row.asciiname) {
      trie.add(row.asciiname, leaf);
    }

    for (var altName of row.alternatenames.split(',')) {
      altName = altName
      if (altName !== name)
        trie.add(altName, leaf);
    }

    // Add full city, region, country name to trie
    let converted = util.convertAdminCode(row);
    trie.add(
      `${row.name}, ${converted.shortName}, ${row.countryCode}`,
      leaf);
    trie.add(
      `${row.name}, ${converted.name}, ${row.countryCode}`,
      leaf);
  });
  return {tree, trie};
}

module.exports = {
  QuadTree,
  fromStream,
  distance
}
