const express = require('express');
const geolib = require('geolib');
const app = express();
const dataUtils = require('./data-utils');
const port = process.env.PORT || 2345;

let citiesData = require('./sync-load-data');
citiesData = dataUtils.makeRegionsReadable(citiesData);
citiesData = dataUtils.renameLatLong(citiesData);

app.get('/suggestions', (req, res) => {
  let potentialCityMatches = [];

  if (req.query.q != null && res.query.q.length >= 1) {
    const queryRegex = new RegExp(`^${req.query.q}.*`, 'i');
    potentialCityMatches = citiesData.filter(cityData => cityData.name.match(queryRegex));
  }

  const latitudeFloat = Number.parseFloat(res.query.latitude);
  const isLatitudeANumber = !Number.isNaN(latitudeFloat);
  const longitudeFloat = Number.parseFloat(res.query.longitude);
  const isLongitudeANumber = !Number.isNaN(longitudeFloat);
  if (isLatitudeANumber && isLongitudeANumber) {
    const userCoord = {
      latitude: latitudeFloat,
      longitude: longitudeFloat
    };
    potentialCityMatches.forEach(cityData => {
      const cityCoord = {
        latitude: cityData.latitude,
        longitude: cityData.longitude
      };
      cityData.distanceInKM = geolib.getDistance(userCoord, cityCoord) / 1000; //distance in kilometers
    })
  }

  if (potentialCityMatches.length > 0) {
    //apply scores
  } else {
    res.status(404);
  }
  res.send({
    suggestions: potentialCityMatches
  });
});

app.use((req, res) => res.sendStatus(404)); //default 404 handler if no route found
app.listen(port, '127.0.0.1', () => console.log('Server running at http://127.0.0.1:%d/suggestions', port));

module.exports = app;