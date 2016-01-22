var fs = require('fs'),
    readline = require('readline'),
    Stream = require('stream'),
    util = require('util');

var Transform = Stream.Transform;

// Calculates the distance between two lat/long pairs
// in KM.
// https://en.wikipedia.org/wiki/Haversine_formula
// http://stackoverflow.com/a/27943
var R = 6371; // Mean radius of the earth in km
function distance(lat1, long1, lat2, long2) {
  var degreeLat = deg2rad(lat2-lat1);  // deg2rad below
  var degreeLong = deg2rad(long2-long1);
  var a =
        Math.sin(degreeLat/2) * Math.sin(degreeLat/2) +
        Math.cos(deg2rad(lat1)) * Math.cos(deg2rad(lat2)) *
        Math.sin(degreeLong/2) * Math.sin(degreeLong/2);
  var c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1-a));
  var d = R * c; // Distance in km
  return d;
}

// lat = y
// long = x
class QuadTree extends Transform {
  constructor() {
    super({objectMode: true})
  }

  _transform(row, enc, done) {
    this.insert(row, undefined)
    done()
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
      console.log('initializing root')

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

      return newLeaf

    } else {
      return this.insert(newLeaf, this.root);
    }
  }

  retrieveClosest(latitude, longitude, parentLeaf) {
    if (parentLeaf) {
      if (parentLeaf.latitude == latitude &&
          parentLeaf.longitude == longitude) {
        return parentLeaf
      }

      // NE
      if (latitude >= parentLeaf.latitude &&
          longitude >= parentLeaf.longitude) {
        // If more tree traversal is possible
        if (parentLeaf.ne)
          return this.retrieveClosest(latitude, longitude, parentLeaf.ne)
      }

      // NW
      else if (latitude >= parentLeaf.latitude &&
               longitude <= parentLeaf.longitude) {
        if (parentLeaf.nw)
          return this.retrieveClosest(latitude, longitude, parentLeaf.nw)
      }

      // SW
      else if (latitude <= parentLeaf.latitude &&
               longitude <= parentLeaf.longitude) {
        if (parentLeaf.sw)
          return this.retrieveClosest(latitude, longitude, parentLeaf.sw)
      }

      // SE
      else if (latitude <= parentLeaf.latitude &&
               longitude >= parentLeaf.longitude) {
        if (parentLeaf.se)
          return this.retrieveClosest(latitude, longitude, parentLeaf.se)
      }

      return parentLeaf

    } else {
      if (this.root)
        return this.retrieveClosest(latitude, longitude, this.root)
      else
        return undefined
    }
  }
}

module.exports = {
  QuadTree: QuadTree
}
