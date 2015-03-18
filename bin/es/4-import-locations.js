var es = require('elasticsearch');

var config = require('../../config/config.json');
var locations  = require('../../data/locations.json');

var client = new es.Client({
  host: config.esHost,
  log: 'trace'
});

var bulkActions = [];

locations.forEach(function (location) {
  // action description
  // challenge/location
  bulkActions.push({ index: { _index: 'challenge', _type: 'location', _id: location.id }});
  bulkActions.push(location);
});
client.bulk({
  body: bulkActions
}).then(function (res) {
  console.log('done', res);
}, function (err) {
  console.log('errrrr', err, arguments);
});