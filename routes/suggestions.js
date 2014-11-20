var express = require('express');
var removeDiacritics = require('diacritics').remove;
var _ = require('lodash');
var geolib = require('geolib');

var City = require('../models/city');
var SuggestionService = require('../services/suggestion_service');

var router = express.Router();

/**
 * GET /suggestion?q=XXX(&latitude=YYY&longitude=ZZZ)
 * 
 * @param {String} q searching query from the user
 * @param {Number} latitude latitude (optional)
 * @param {Number} latitude longitude (optional)
 */
router.get('/suggestions', function(req, res, next) {
  var q = req.query.q || '';
  q = decodeURIComponent(q).trim();
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

    // records found: compute scores
    var criteriaScoreMap = SuggestionService.computeAbsoluteScoreMap(
      {q:q, longitude: longitude, latitude: latitude},
      cities);

    // get highest score
    var highestScore = SuggestionService.getHighestScore(criteriaScoreMap);

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

      // normalize its score
      var score = (criteriaScoreMap[city]/highestScore).toFixed(3);

      // build dto
      var dto = {
        name: fullName,
        latitude: '' + city[City.LATITUDE_FIELD],
        longitude: '' + city[City.LONGITUDE_FIELD],
        score: score
      };
      dtos.push(dto);
    });

    // sort by score DESC
    dtos = _.sortBy(dtos, function(dto) {
      return -dto.score;
    });

    // end
    res
      .json({ suggestions: dtos })
      .end();
  });
});

module.exports = router;
