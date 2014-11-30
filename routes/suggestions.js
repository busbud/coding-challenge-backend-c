'use strict';

var url = require('url');
var logger = require('../helpers/logger'),
    helpersSendOK = require('../helpers/responses/sendOK'),
    helpersNotFound = require('../helpers/responses/notFound'),
    parseQueryString = require('../helpers/parse_query_string'),
    calculateScore = require('../helpers/calculate_score');

// suggestions Handler
//
// @method suggestions
// @param {Object} req
// @param {Object} res
//
// Usage:
// return suggestions(req, res);
module.exports = function suggestions(req, res, cities) {
  var log = logger('suggestions');

  var queryString = url.parse(req.url, true).query;
  var query;

  try {
      query = parseQueryString(queryString);
  } catch (e) {
      log("Error when parsing query %s", e);
      helpersNotFound(res);
      return;
  }

  //TODO: cache in memcache

  var suggested = [],
      result = [],
      maxDistance = null,
      maxPopulation = null;
  for (var index in cities) {
    var city = cities[index];
    if (city.name.indexOf(query.q) === 0 || city.ascii.indexOf(query.q) === 0 ) {
      var name = [];
      if(city.name) name.push(city.name);
      if(city.province) name.push(city.province.toUpperCase());
      if(city.country) name.push(city.country);

      var i = {
        "name": name.join(', '),
        "rawName": city.name,
        "latitude": city.lat,
        "longitude": city.long,
        "population": city.population,
      };
      if(query.lat != null && query.long != null) {
        i.distance = city.distance(query.lat, query.long);
        if(maxDistance === null || i.distance >= maxDistance) maxDistance = i.distance;
      }
      if(maxPopulation === null || i.population >= maxPopulation) maxPopulation = i.population;
      suggested.push(i);
    }
  }

  suggested.forEach(function (city) {
    city.score = calculateScore(city, maxPopulation, maxDistance, query.q);
    delete city.distance;
    delete city.population;
    delete city.rawName;
    result.push(city);
  });

  var result = result.sort(function (itemA, itemB) {
      return (itemA.score - itemB.score);
  });

  helpersSendOK(res, result.reverse());
  return;
};