var path = require('path');
var fs = require('fs');
var locationParser = require('../../lib/location-parser');

// var dataFile = path.join(_dirname, './data/cities_canada.tsv');
var dataFile = path.join(__dirname, '../../data/test_cities.tsv');
// @TODO: stream it!
var rawLocations = []
locationParser.load(dataFile, function (err, locations) {
  if (err) {throw err;}
  locations.forEach(function (loc) {
    rawLocations.push({
      id: loc.id,
      name: loc.name,
      country: loc.country,
      state: loc.state,
      coord: loc.latitude + ','  + loc.longitude
    });
  });
  console.log(JSON.stringify(rawLocations));
  fs.writeFile(path.join(__dirname, '../../data/locations.json'), JSON.stringify(rawLocations));
  
});
