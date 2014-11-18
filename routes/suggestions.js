var express = require('express');
var removeDiacritics = require('diacritics').remove;
var _ = require('lodash');
var geolib = require('geolib');

var City = require('../models/city');

var router = express.Router();

router.get('/suggestions', function(req, res, next) {
  var q = req.query.q || '';
  var latitude = req.query.latitude || '';
  var longitude = req.query.longitude || '';

  // empty query string
  if (q === '') {
    res
      .status(404)
      .json({ suggestions: [] });
    return;
  }

  // args for query
  var params = {
    conditions: {},
    sort: '-score ' + City.NAME_FIELD,
    limit: 1000
  };
  // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions#Using_Special_Characters
  var sanitizedQ = q.replace(/([.*+?^${}()|\[\]\/\\])/g, "\\$1");
  // https://www.npmjs.org/package/diacritics
  var qNoDiacritics = removeDiacritics(sanitizedQ);
  // WHERE City.ASCII_FIELD = regexp(/q/i)
  params['conditions'][City.ASCII_FIELD] = new RegExp(qNoDiacritics, 'i');

  // query data
  City.cityBeLike(params, function(err, cities) {
    if (err) return next(err);

    // no records found
    if (cities.length === 0) {
      res
        .status(404)
        .json({ suggestions: [] });
      return;
    }

    // records found
    // optional step: longitude/latitudes, both must be numeric values
    if (!isNaN(latitude) && !isNaN(longitude)) {
      // sort by geolocation distance asc
      cities = _.sortBy(cities, function(city) {
        // https://www.npmjs.org/package/geolib
        return geolib.getDistance(
          { latitude: city[City.LATITUDE_FIELD], longitude: city[City.LONGITUDE_FIELD] },
          { latitude: Number(latitude), longitude: Number(longitude) }
        );
      })
    }

    // [entity] -> [dto]
    var dtos = [];
    _.forEach(cities, function(city) {
      var name = city[City.NAME_FIELD];
      var country = city[City.COUNTRY_FIELD];

      // build name field
      var fullName = name
        .concat(', ')
        .concat(City.lookupAdmin1(city))
        .concat(', ')
        .concat(City.country[country]);

      // build dto
      var dto = {
        name: fullName,
        latitude: city[City.LATITUDE_FIELD],
        longitude: city[City.LONGITUDE_FIELD],
        score: 1337 // TODO
      };
      dtos.push(dto);
    });

    // end
    res
      .json({ suggestions: dtos })
      .end();
  });
});

module.exports = router;
