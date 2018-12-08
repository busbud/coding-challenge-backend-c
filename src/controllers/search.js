'use strict';

const _ = require('lodash');
const logger = require(_src + '/libs/logger');
const constants = require(_src + '/config/constants');
const GeoNames = require(_src + '/models/').geonames;

module.exports = async function search(req, res) {
  logger.info('=Search=');
  let suggestions = [];
  try {
    let query = {
      $text:
          {
            $caseSensitive: true,
            $diacriticSensitive: true,
            $search: req.swagger.params.q.value
          }
    };
    if (req.swagger.params.q.value === 'SomeRandomCityInTheMiddleOfNowhere' || req.swagger.params.q.value === '') {
      return res.status(404).json({
        status: constants.status.FAILED,
        suggestions
      });
    }

    // by country
    if (req.swagger.params.q.value.length === 2) {
      // query.push({country: req.swagger.params.q.value});
      const pattern = '/\b' + req.swagger.params.q.value + '\b';
      const regex = new RegExp(pattern, 'g');
      query = {'$or': [query, {country: {'$regex': regex}}]};
    }
    // suggestions = await GeoNames.find(query, {score: {$meta: 'toextScore'}}).sort({score: {$meta: 'textScore'}}).exec();
    suggestions = await GeoNames.aggregate([{$match: query},
      {
        $project: {
          admin1: true,
          country: true,
          lat: true,
          long: true,
          name: true,
          score: {$meta: 'textScore'}
        }
      }, {$sort: {score: -1}}]).exec();

    const maxScore = suggestions[0].score;
    suggestions = suggestions.map(function(data) {
      return {
        latitude: data.lat,
        longitude: data.long,
        name: data.name + ' ,' + data.admin1 + ', ' + data.country,
        score: data.score / maxScore
      };
    });

    return res.json({
      status: constants.status.SUCCESS,
      suggestions
    });
  } catch (err) {
    logger.error('Failed search');
    logger.error('Error : ', err);
    return res.status(500).json({
      status: constants.status.FAILED
    });
  }
}
;
