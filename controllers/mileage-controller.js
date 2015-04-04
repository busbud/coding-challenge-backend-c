// Internal dependencies
var mileageService = require('../services/mileage-service');
var logger = require('../utils/logger');
var _ = require('../utils/underscore_with_string');


var mileageController = exports;


function computeDistance(req, res) {

  var departure_latitude = req.query.departure_latitude;
  var departure_longitude = req.query.departure_longitude;
  var arrival_latitude = req.query.arrival_latitude;
  var arrival_longitude = req.query.arrival_longitude;

  var dep_latitude_exists = !_.str.isBlank(departure_latitude);
  var dep_longitude_exists = !_.str.isBlank(departure_longitude);
  var arr_latitude_exists = !_.str.isBlank(arrival_latitude);
  var arr_longitude_exists = !_.str.isBlank(arrival_longitude);

  var isDepartureParamsOk = (dep_latitude_exists && dep_longitude_exists);
  var isArrivalParamsOk = (arr_latitude_exists && arr_longitude_exists);

  if (!isDepartureParamsOk || !isArrivalParamsOk) {
    logger
      .info('MileageController#computeDistance -- Bad params given');
    res
      .status(400)
      .json({
        error: 'Bad params given, needed latitude and longitude for departure and arrival'
      });
    return;
  }

  var opts = {
    departure: {
      latitude: departure_latitude,
      longitude: departure_longitude
    },
    arrival: {
      latitude: arrival_latitude,
      longitude: arrival_longitude
    }
  };

  mileageService
    .computeDistance(opts)
    .then(function (result) {

      logger
        .info('MileageController#computeDistance -- Request well processed');
      res
        .status(200)
        .json(result);

    })
    .catch(function (err) {

      logger
        .debug('MileageController#computeDistance -- catched error ' +
          err.message);

      res
        .status(500)
        .send({
          error: 'Oops... An internal error occured on the server'
        });
    })
    .done();

}


mileageController.computeDistance = computeDistance;
