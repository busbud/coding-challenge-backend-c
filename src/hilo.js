var fs = require('fs'),
    readline = require('readline'),
    Stream = require('stream'),
    util = require('util'),
    tsv = require('./tsv');

var Transform = Stream.Transform;

var lolatitude = 90000000.0;
var hilatitude = 0.0;
var lolongitude = 9000000.0;
var hilongitude = -10000000.0;

class Hilo extends Transform {
  constructor() {
    super({objectMode: true});
    this.first = true;
  }

  _transform(row, enc, done) {
    // ignore header
    if (this.first) {
      this.first = false
    } else {
      if (row.latitude < lolatitude) {
        lolatitude = row.latitude
      } else if (row.latitude > hilatitude) {
        hilatitude = row.latitude
      }

      if (row.longitude < lolongitude) {
        lolongitude = row.longitude
      } else if (row.longitude > hilongitude) {
        hilongitude = row.longitude
      }
    }

    done()
  }
}

var rows = tsv.fromFile('data/cities_canada-usa.tsv');

rows.pipe(new Hilo())
process.on('exit', function() {
  console.log(lolatitude)
  console.log(hilatitude)
  console.log(lolongitude)
  console.log(hilongitude)
})
