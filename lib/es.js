var es = require('elasticsearch');
var _ = require('lodash');

var Location = require('./location');

var config = require('../config/config.json');

var client = new es.Client({
  host: config.esHost/*,
  log: 'trace'*/
});

function createLocation(result) {
  var coords = result._source.coord.split(',');
  return new Location(_.extend(result._source, {
    latitude: coords[0],
    longitude: coords[1]
  }));
}

module.exports = {
  find: function (query, cb) {
    client.search({
      index: 'challenge',
      body: {
        from: 0, size: 5,
        query:{
          fuzzy : { name: query.q }
        }
      }
    }, function (err, response) {
      if (err) { return err(err); }
      if (response.hits && response.hits.hits) {
        return cb(null, response.hits.hits.map(createLocation));
      }
      cb(null, []);
      // if (!response._hits || !response.hits._hits)
    }, cb);
  }
}