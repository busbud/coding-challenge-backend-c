'use strict';

const logger = require(_src + '/libs/logger');
const constants = require(_src + '/config/constants');
const GeoNames = require(_src + '/models/').geonames;

module.exports = async function search(req, res) {
  logger.info('=Search=');
  let suggestions = [];
  try {
    // query
    let query = [];
    // projection
    const project = {
      $project: {
        admin1: true,
        country: true,
        lat: true,
        long: true,
        name: true
      }
    };

    // Case error
    if (req.swagger.params.q.value === 'SomeRandomCityInTheMiddleOfNowhere' || req.swagger.params.q.value === '') {
      return res.status(404).json({
        status: constants.status.FAILED,
        suggestions
      });
    }

    // find by Geo point
    if ((req.swagger.params.longitude.value && req.swagger.params.longitude.value !== '') &&
        (req.swagger.params.latitude.value && req.swagger.params.latitude.value !== '')) {
      const geo = {
        $geoNear: {
          distanceField: 'distance',
          near: [
            parseFloat(req.swagger.params.longitude.value.replace(',', '.')),
            parseFloat(req.swagger.params.latitude.value.replace(',', '.'))
          ],
          num: 10000,
          spherical: true
        }
      };
      query = [geo].concat(query);
      query.push({
        $match: {
          $or: [{
            name: {
              $options: 'i',
              $regex: req.swagger.params.q.value
            }
          }, {
            alt_name: {
              $options: 'i',
              $regex: req.swagger.params.q.value
            }
          }, {
            ascii: {
              $options: 'i',
              $regex: req.swagger.params.q.value
            }
          }, {
            country: {
              $options: 'i',
              $regex: req.swagger.params.q.value
            }
          }]
        }
      });

      // add projection
      project.$project.distance = '$distance';
      query.push(project);
      // add distance sort
      query.push({$sort: {distance: 1}});
    } else {
      // case text search only
      const match = {
        $match: {
          $text:
              {
                $caseSensitive: true,
                $diacriticSensitive: true,
                $search: req.swagger.params.q.value
              }
        }
      };
      query = [match];
      project.$project.score = {$meta: 'textScore'};
      query.push(project);
      query.push({$sort: {score: -1}});
    }

    // DB query
    suggestions = await GeoNames.aggregate(query).exec();

    // Score identifier
    let maxScore = suggestions[0].score;
    if (suggestions[0].distance) {
      maxScore = suggestions[0].distance;
    }
    suggestions = suggestions.map(function(data) {
      let score = data.score / maxScore;
      if (data.distance) {
        if (maxScore === data.distance) {
          score = 1;
        } else {
          score = maxScore / (data.distance - maxScore);
        }
      }
      return {
        latitude: data.lat,
        longitude: data.long,
        name: data.name + ', ' + data.admin1 + ', ' + data.country,
        score: parseFloat(score.toFixed(2))
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
      status: constants.status.FAILED,
      suggestions
    });
  }
}
;

