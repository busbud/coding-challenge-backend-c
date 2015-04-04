// Internal dependencies
var suggestionsService = require('../services/suggestions-service');
var logger = require('../utils/logger');
var _ = require('../utils/underscore_with_string');


var suggestionsController = exports;


function suggestByName(req, res) {

  var p_name = req.query.q;
  if (_.isBlank(p_name)) {
    logger
      .info('SuggestionsController#suggestByName -- No name given');
    res
      .status(400)
      .json({
        error: 'Bad params given, need at least a name for the city'
      });
    return;
  }

  var p_latitude = req.query.latitude;
  var p_longitude = req.query.longitude;

  var p_latitude_exists = !!p_latitude;
  var p_longitude_exists = !!p_longitude;
  if (p_latitude_exists !== p_longitude_exists) {
    logger
      .info('SuggestionsController#suggestByName -- Cannot receive a' +
        'Latitude without the longitude or longitude without latitude');
    res
      .status(400)
      .json({
        error: 'Bad params given, given both latitude and longitude or no ones'
      });
    return;
  }

  var opts = {
    name: p_name,
    latitude: p_latitude,
    longitude: p_longitude
  };

  suggestionsService
    .suggestCityForOptions(opts)
    .then(function (cities) {

      if (_.isEmpty(cities.suggestions)) {
        logger
          .info('SuggestionsController#suggestByName -- No result found');
        res
          .status(404)
          .json(cities);
        return;
      }

      logger
        .info(
          'SuggestionsController#suggestByName -- Request well processed');
      res
        .status(200)
        .json(cities);

    })
    .catch(function (err) {

      logger
        .debug('SuggestionsController#suggestByName -- catched error ' +
          err.message);

      res
        .status(500)
        .send({
          error: 'Oops... An internal error occured on the server'
        });
    })
    .done();

}


suggestionsController.suggestByName = suggestByName;
