import Geocode from '../geocode';
var stream = require('stream');
var util = require('util');
var StringDecoder = require('string_decoder').StringDecoder;

const geocode = new Geocode();

// node v0.10+ use native Transform, else polyfill
var Transform = stream.Transform || require('readable-stream').Transform;

export function Mapper(options) {
  // allow use without new
  if (!(this instanceof Mapper)) {
    return new Mapper(options);
  }

  // init Transform
  Transform.call(this, options);
}
util.inherits(Mapper, Transform);

Mapper.prototype._transform = function(chunk, enc, cb) {
  this.push({
    name: `${chunk.name}, ${geocode.getRegion(
      chunk.country,
      chunk.region
    )}, ${geocode.getCountry(chunk.country)}`,
    latitude: chunk.latitude,
    longitude: chunk.latitude,
    score: chunk.score
  });
  cb();
};
