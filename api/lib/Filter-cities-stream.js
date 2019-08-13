const stream = require('stream');
const util = require('util');
const geoScore = require('./geo-score');
const removeAccents = require('remove-accents');


// node v0.10+ use native Transform, else polyfill
const Transform = stream.Transform || require('readable-stream').Transform;

function FilterCitiesStream(filterProps, options) {
  // allow use without new
  if (!(this instanceof FilterCitiesStream)) {
    return new FilterCitiesStream(filterProps, options);
  }

  // init Transform
  if (!options) options = {}; // ensure object
  options.objectMode = true; // forcing object mode
  Transform.call(this, options);
  this.fields = filterProps['fields'];
  this.stringToMatch = removeAccents(filterProps['match']);
  this.latitude = filterProps['latitude'];
  this.longitude = filterProps['longitude'];
}
util.inherits(FilterCitiesStream, Transform);

/* filter each object's sensitive properties */
FilterCitiesStream.prototype._transform = function(obj, enc, cb) {
  obj['name'] = removeAccents(obj['name']);
  if (obj['name'].indexOf(this.stringToMatch) !== 0) {
    cb();
    return;
  }

  obj['score'] = geoScore.calcScore(this.latitude,
    this.longitude,
    obj['latitude'],
    obj['longitude'],
    this.stringToMatch,
    obj['name']

  );
  obj['name'] = `${obj['name']}, ${obj['prov_state']}, ${obj['country']}`;
  var self = this;
  // determine what keys to keep
  var filteredKeys = Object.keys(obj).filter(
    function(key) {
      // only those keys  in this list
      return (self.fields.indexOf(key) >= 0);
    }
  );

  // create clone with only these keys
  var filteredObj = filteredKeys.reduce(
    function(accum, key) {
      accum[key] = obj[key];
      return accum;
    },
    {}
  );

  this.push(filteredObj);
  cb();
};

module.exports = FilterCitiesStream;
