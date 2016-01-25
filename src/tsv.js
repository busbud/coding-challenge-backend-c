"use strict";
var fs = require('fs'),
    readline = require('readline'),
    Stream = require('stream'),
    util = require('util');

var Transform = Stream.Transform;

// Splits the incoming stream into lines
class LineParse extends Transform {
  constructor() {
    super({objectMode: true});
  }

  _transform(chunk, enc, cb) {
    let data = chunk.toString();
    if (this.prevData) {
      data = this.prevData + data;
    }

    const lines = data.split('\n');
    this.prevData = lines.splice(lines.length-1,1)[0];
    lines.forEach(this.push.bind(this));
    cb();
  }
}

// Transforms a raw tsv stream to a stream of objects
class RowParse extends Transform {
  constructor() {
    super({objectMode: true});
    this.first = true;
  }

  // Expects each chunk to be a line (String)
  _transform(line, enc, cb) {
    if (this.first) {
      this.first = false
    } else {
      const row = RowParse.parseRowLine(line);
      this.push(row);
    }
    cb();
  }

  // representing each row of the file.
  // Returns a stream of objects
  static parseRowLine(line) {
    const parts = line.split('\t');
    const row = {
      // geonameid: parts[0],
      name: parts[1],
      asciiname: parts[2],
      alternatenames: parts[3],
      latitude: parseFloat(parts[4]),
      longitude: parseFloat(parts[5]),
      // featureClass: parts[6],
      // featureCode: parts[7],
      countryCode: parts[8],
      // cc2: parts[9],
      admin1Code: parts[10],
      // admin2Code: parts[11],
      // admin3Code: parts[12],
      // admin4Code: parts[13],
      population: parseInt(parts[14]),
      // elevation: parseInt(parts[15]),
      // dem: parseInt(parts[16]),
      // timezone: parts[17],
      // modificationDate: undefined
    };
    // const rawDate = parts[18].split('-');
    // row.modificationDate = new Date(rawDate[0], rawDate[1], rawDate[2]);

    return row;
  }
}

// Used for logging purposes.
// process.stdout doesn't like objectMode streams.
// So, we have to stringify it.
class Stringify extends RowParse {
  _transform(obj, enc, cb) {
    this.push(JSON.stringify(obj) + '\n')
    cb();
  }
}

// Parses a TSV file
// Returns a stream of objects
function fromFile(filename) {
  return fs.createReadStream(filename)
    .pipe(new LineParse())
    .pipe(new RowParse())
    // .pipe(new Stringify())
    // .pipe(process.stdout);
}

module.exports = {
  fromFile: fromFile,
  RowParse: RowParse,
  Stringify: Stringify
}
